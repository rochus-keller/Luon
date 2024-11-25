/*
** Copyright (C) 2024 Rochus Keller (me@rochus-keller.ch)
**
** This file is part of the Luon language project.
**
**
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file. Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

#include "LnValidator.h"
#include "LnToken.h"
#include <bitset>
#include <limits>
#include <cmath>
#include <QtDebug>
using namespace Ln;

Validator::Validator(AstModel* mdl, Importer* imp, bool xref):mdl(mdl),imp(imp),first(0),last(0)
{
    Q_ASSERT(mdl);
    if( xref )
        first = last = new Symbol();
}

Validator::~Validator()
{
    if( first )
        Symbol::deleteAll(first);
}

bool Validator::validate(Declaration* module, const Import& import)
{
    Q_ASSERT(module);
    this->module = module;
    if( first )
    {
        first->decl = module;
        first->kind = Symbol::Module;
        first->pos = module->pos;
        first->len = module->name.size();
    }

    markDecl(module);

    Ln::ModuleData md = module->data.value<Ln::ModuleData>();

    try
    {

    md.metaActuals = import.metaActuals;
    md.path = import.path;
    if( imp )
        md.suffix = imp->moduleSuffix(import.metaActuals);
    if( !md.suffix.isEmpty() )
    {
        md.fullName = Ln::Token::getSymbol(md.path.join('/') + md.suffix);
        module->name = Ln::Token::getSymbol(module->name + md.suffix);
    }else
        md.fullName = Ln::Token::getSymbol(md.path.join('/'));
    module->data = QVariant::fromValue(md);

    scopeStack.push_back(module);
    for( int i = 0; i < md.metaParams.size(); i++ )
        visitDecl(md.metaParams[i]);
    scopeStack.pop_back();
    if( !import.metaActuals.isEmpty() )
    {
        QString importer;
        if( import.importer )
            importer = import.importer->data.value<ModuleData>().source;
        // trying to instantiate
        if( md.metaParams.size() != md.metaActuals.size() )
        {
            errors << Error("number of formal and actual meta parameters doesn't match",
                            import.importedAt, importer);
            return false;
        }
        for( int i = 0; i < md.metaParams.size(); i++ )
        {
            Type* mt = deref(md.metaParams[i]->type);
            Expression* ma = md.metaActuals[i];
#if 0
            // TODO: doesn't work yet because procedure types/consts not yet completely validated
            // Neither seems to be necessary since when not compatible and used in the generic code
            // the incompatibility will be detected there
            if( mt && mt->form != Type::Generic && !assigCompat(mt, ma) )
                errors << Error("actual meta type not compatible with meta parameter type",
                                ma->pos, import.importer);
#endif
            if( md.metaParams[i]->kind == Declaration::TypeDecl )
            {
                // No, we have to always replace the type.
                // otherwise the wrong type is created when doing new(T) in the generic, and there
                // is a loophole in type checking
                // if( mt->form == Type::Generic )
                {
                    if( md.metaParams[i]->type && md.metaParams[i]->ownstype )
                        delete md.metaParams[i]->type;
                    md.metaParams[i]->type = ma->type;
                    md.metaParams[i]->ownstype = false;
                }
            }else
            {
                if( !ma->isConst() )
                    errors << Error("expecting a const expression", ma->pos, importer);
#if 0
                if( mt->form < BasicType::StrLit && mt->form > BasicType::ANYREC )
                    // don't support proc refs as meta params for now because of circular references
                    errors << Error("only const meta parameters of basic type are supported", ma->pos, importer);
#endif
                // if( mt->form == Type::Generic )
                {
                    if( md.metaParams[i]->type && md.metaParams[i]->ownstype )
                        delete md.metaParams[i]->type;
                    md.metaParams[i]->type = ma->type;
                    md.metaParams[i]->ownstype = false;
                }
                md.metaParams[i]->data = ma->val;
            }
        }
    }

    visitScope(module);
    }catch(...)
    {
    }

    if( first )
        last->next = first; // close the circle
    return errors.isEmpty();
}

Xref Validator::takeXref()
{
    Xref res;
    if( first == 0 )
        return res;
    res.uses = xref;
    res.syms = first;
    res.subs = subs;
    xref.clear();
    subs.clear();
    first = last = new Symbol();
    return res;
}

void Validator::error(const RowCol& pos, const QString& msg) const
{
    ModuleData md = module->data.value<ModuleData>();
    errors << Error(msg, pos, md.source);
}

void Validator::visitScope(Declaration* scope)
{
    scopeStack.push_back(scope);
    Declaration* cur = scope->link;
    while( cur )
    {
        // all decls including procedure headers
        visitDecl(cur);
        cur = cur->getNext();
    }

    QList<Declaration*> bounds_ = boundProcs;
    boundProcs.clear();
    foreach(Declaration* p, bounds_)
    {
        Q_ASSERT(p->link && p->link->kind == Declaration::ParamDecl &&
                 p->link->mode == Declaration::Receiver && p->link->type );
        if( p->link->type->deref()->base )
        {
            Declaration* super = p->link->type->deref()->base->deref()->find(p->name);
            if( super )
            {
                super->hasSubs = true;
                p->super = super;
                if( !matchFormals(super->getParams(true), p->getParams(true)) || !matchResultType(super->type,p->type) )
                    error(p->pos, "formal parameters do not match with the overridden procedure");
                if( first )
                    subs[super].append(p);
            }
        }
        visitScope(p);
    }
    cur = scope->link;
    while( cur )
    {
        if( cur->kind == Declaration::Procedure )
            visitScope(cur);
        cur = cur->getNext();
    }
    visitBody(scope->body);
    scopeStack.pop_back();
}

void Validator::visitDecl(Declaration* d)
{
    if( d->validated )
        return;
    d->validated = true;
    markDecl(d);
    switch( d->kind )
    {
    case Declaration::TypeDecl:
        visitType(d->type);
        if( first && d->type && d->type->form == Type::Record && d->type->base )
        {
            Q_ASSERT( d->type->base->form == Type::NameRef );
            if( !d->type->base->validated )
                break;
            Declaration* super = d->type->base->base->decl;
            super->hasSubs = true;
            d->super = super;
            subs[super].append(d);
        }
        break;
    case Declaration::VarDecl:
    case Declaration::LocalDecl:
    case Declaration::ParamDecl:
    case Declaration::Field:
        visitType(d->type);
        break;
    case Declaration::ConstDecl:
        if(d->expr)
        {
            visitExpr(d->expr);
            d->type = d->expr->type;
            d->data = d->expr->val;
        }else if( d->type == 0 )
            d->type = mdl->getType(BasicType::NoType);
        else
            visitType(d->type);
        break;
    case Declaration::Import:
        visitImport(d);
        break;
    case Declaration::Procedure: {
        // only header is evaluated here
        visitType(d->type);
        const QList<Declaration*> params = d->getParams();
        for( int i = 0; i < params.size(); i++ )
            visitDecl(params[i]);
        break;
        }
    default:
        Q_ASSERT(false);
        break;
    }
}

void Validator::visitImport(Declaration* import)
{
    // TEST import->pos.d_row == 470 && module->name == "Havlak")
    Import i = import->data.value<Import>();
    foreach( Expression* e, i.metaActuals )
        visitExpr(e);

    if( import->outer->kind != Declaration::Module )
        error(import->pos,"imports are only supported on module level");

    Declaration* mod = 0;
    if( imp )
        mod = imp->loadModule(i);
    if( mod )
    {
        // loadModule returns the module decl; we just need the list of module elements:
        mod->hasSubs = true; // the module is used by at least this one
        import->link = mod->link;
        i.resolved = mod;
        import->data = QVariant::fromValue(i);
        markRef(mod, i.importedAt);
    }else
    {
        error(import->pos,"cannot import module");
        throw "";
    }
    import->validated = true;
}

void Validator::visitBody(Statement* s)
{
    // already in the right scope when called

    while( s )
    {
        switch(s->kind)
        {
        case Statement::Assig:
            assigOp(s);
            break;
        case Statement::Call:
            callOp(s);
            break;
        case Statement::If:
            s = ifOp(s);
            break;
        case Statement::Case:
            s = caseStat(s);
            break;
        case Statement::Loop:
            loopStack.push_back(s);
            visitBody(s->body);
            loopStack.pop_back();
            break;
        case Statement::While:
        case Statement::Repeat:
            loopStat(s);
            break;
        case Statement::Exit:
            if( loopStack.isEmpty() )
                error(s->pos, "exit statement requires a surrounding loop statement");
            break;
        case Statement::Return:
            returnOp(s);
            break;
        case Statement::ForAssig:
            s = forStat(s);
            break;
        case Statement::ForToBy:
        case Statement::CaseLabel:
        case Statement::TypeCase:
        case Statement::Elsif:
        case Statement::Else:
            break; // ignore, only visited in case of error
        case Statement::End:
            // NOP
            break;
        default:
            Q_ASSERT(false);
        }
        s = s->getNext();
    }
}

void Validator::visitExpr(Expression* e, Type* hint, bool doNext)
{
    if( e == 0 )
        return;

    switch( e->kind )
    {
    case Expression::Plus:
    case Expression::Minus:
    case Expression::Not: // Unary
        visitExpr(e->lhs,hint);
        unaryOp(e);
        break;
    case Expression::Eq:
    case Expression::Neq:
    case Expression::Lt:
    case Expression::Leq:
    case Expression::Gt:
    case Expression::Geq:
    case Expression::In: // Relation
    case Expression::Is: // Relation
    case Expression::Add:
    case Expression::Sub:
    case Expression::Or: // AddOp
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod:
    case Expression::And: // MulOp
        visitExpr(e->lhs);
        visitExpr(e->rhs);
        binaryOp(e);
        break;
    case Expression::Literal:
    case Expression::DeclRef:
    case Expression::Cast:
    case Expression::KeyValue:
        resolve(e->type);
        break;
    case Expression::NameRef: {
        const Qualident q = resolve(e);
        if( !q.second.isEmpty() )
        {
            // the nameref was not a true qualident (i.e. not referencing an import),
            // but a select referencing a field
            Expression* nameRef = new Expression();
            *nameRef = *e; // repurpose the new expr as the validated nameref
            e->kind = Expression::Select;
            e->val = q.second;
            e->lhs = nameRef;
            e->pos.d_col += q.first.size() + 1; // first.second
            nameRef->next = 0;
            selectOp(e);
        }
        break;
        }
    case Expression::Select:
        visitExpr(e->lhs);
        selectOp(e);
        break;
    case Expression::Index:
        visitExpr(e->lhs);
        visitExpr(e->rhs);
        indexOp(e);
        break;
    case Expression::Call:
        if( e->lhs && e->lhs->kind == Expression::Super )
            visitExpr(e->lhs->lhs);
        else
            visitExpr(e->lhs); // proc
        callOp(e);
        break;
    case Expression::Super:
        error(e->pos,"super call cannot be used here");
        break;
    case Expression::Constructor:
        constructor(e,hint);
        break;
    case Expression::Range: // used in Constructor and LabelRange (as ConstExpr)
        visitExpr(e->lhs);
        visitExpr(e->rhs);
        e->type = e->lhs->type;
        break;
    case Expression::Invalid:
        Q_ASSERT(false);
        break;
    }
    if( doNext && e->next )
        visitExpr(e->next);
}

void Validator::visitType(Type* type)
{
    if( type == 0 || type->validated )
        return;
    type->validated = true;
    switch( type->form )
    {
    case Type::ConstEnum:
        visitEnum(type);
        break;
    case Type::Record:
    case Type::Proc:
        foreach( Declaration* d, type->subs )
        {
            if( d->kind == Declaration::Procedure )
                boundProcs << d; // do body later
            visitDecl(d);
        }
        visitType(type->base);
        if( type->form == Type::Record && type->base )
        {
            if( type->base->base == 0 || type->base->deref()->form != Type::Record )
                error(type->base->decl->pos,"invalid base record");
        }
        break;
    case Type::Array:
    case Type::HashMap:
        visitExpr(type->expr );
        if( type->form == Type::Array )
        {
            if( type->expr )
            {
                if( !type->expr->isConst() || !deref(type->expr->type)->isInteger() )
                    error(type->expr->pos,"expecting constant integer expression");
                else
                {
                    const qint64 tmp = type->expr->val.toLongLong();
                    if( tmp < 0 || tmp > std::numeric_limits<quint32>::max() )
                        error(type->expr->pos,"invalid array length");
                    else
                        type->len = tmp;
                }
            }
        }else if( type->form == Type::HashMap && type->expr->kind == Expression::DeclRef )
        {
            if( type->expr->val.value<Declaration*>()->kind != Declaration::TypeDecl )
                error(type->expr->pos,"expecting a type name");
        }
        visitType(type->base);
        break;
    case Type::NameRef:
        // we no longer replace NameRefs by the original type, because doing so requires essentially to
        // completeley treat each declaration and expression in the first batch; if we then don't succeed
        // to resolve the type (e.g. because we resolve meta actuals and the resolution chain finally depends
        // on exactly this import to be completed), we would have to run several iterations to find all yet unresolved
        // types, or it's not even possible to break circular dependencies. If we leaf NameRefs instead, we
        // just pass the NameRef to the declarations and expressions as their types and come back later to
        // complete the process. The price is that we need deref.
        type->validated = false;
        resolve(type);
        break;
    }
}

void Validator::visitEnum(Type* t)
{
    qint64 index = 0;
    foreach( Declaration* d, t->subs )
    {
        Q_ASSERT(d->kind == Declaration::ConstDecl);
        if( d->expr )
        {
            visitExpr(d->expr);
            if( !d->expr->isConst() )
                error(d->expr->pos,"expecting a constant expression");
            else if( d->type && !d->type->deref()->isInteger() )
                error(d->expr->pos,"expecting an integer");
            else
                index = d->expr->val.toLongLong();
        }
        d->data = index;
        index++;
    }
}

void Validator::resolve(Type* nameRef)
{
    if( nameRef == 0 || nameRef->form != Type::NameRef)
        return;
    if( nameRef->validated )
        return;
    Q_ASSERT(nameRef->decl);
    Q_ASSERT(nameRef->expr == 0);
    Qualident q = nameRef->decl->data.value<Qualident>();
    ResolvedQ r = find(q, nameRef->decl->pos);
    if(r.second == 0)
        return;
    RowCol pos = nameRef->decl->pos;
    if( r.first != 0 )
    {
        markRef(r.first, pos);
        pos.d_col += q.first.size() + 1;
    }
    markRef(r.second, pos);
    nameRef->validated = true;
    nameRef->base = r.second->type;
    if( r.second->kind != Declaration::TypeDecl )
        return error(nameRef->decl->pos,"identifier doesn't refer to a type declaration");
    resolve(r.second->type);
    if( r.second->type )
    {
#if 0
        if( d->type->form == Type::Proc )
            visitType(d->type); // RISK: requred so that const meta params are validated when instantiating
            // does not work yet because it triggers circular dependencies
        else
#endif
            resolve(r.second->type->base);
    }
}

static inline void toConstVal(Expression* e)
{
    if(e->lhs)
    {
        delete e->lhs;
        e->lhs = 0;
    }
    if( e->rhs )
    {
        delete e->rhs;
        e->rhs = 0;
    }
    e->kind = Expression::ConstVal;
}

void Validator::unaryOp(Expression* e)
{
    if( e->lhs->type == 0 )
        return; // already reported
    Type* lhsT = deref(e->lhs->type);
    if( e->kind == Expression::Plus )
    {
        if( !lhsT->isNumber() )
            return error(e->pos, "unary operator not applicable to this type");
    }else if( e->kind == Expression::Minus )
    {
        if( !lhsT->isNumber() && !lhsT->isSet() )
            return error(e->pos, "unary operator not applicable to this type");
    }else if( e->kind == Expression::Not )
    {
        if( !lhsT->isBoolean()  )
            return error(e->pos, "unary '~' or 'NOT' not applicable to this type");
    }
    if( e->lhs == 0 )
        return; // already reported
    e->type = lhsT;

    switch( e->kind )
    {
    case Expression::Not:
        if( e->lhs->isConst() )
        {
            e->val = !e->lhs->val.toBool();
            toConstVal(e);
        }
        break;
    case Expression::Plus:
        if( e->lhs->isConst() )
        {
            e->val = e->lhs->val;
            toConstVal(e);
        }
        break;
    case Expression::Minus:
        if( e->lhs->isConst() )
        {
            if( lhsT->isNumber()  )
                e->val = -e->lhs->val.toDouble();
            else
                e->val = ~e->lhs->val.toUInt();
            toConstVal(e);
        }
        break;
    default:
        Q_ASSERT(false);
        break;
    }
}

void Validator::binaryOp(Expression* e)
{
    if( e->lhs == 0 || e->rhs == 0 || e->lhs->type == 0 || e->rhs->type == 0 )
        return; // already reported

    switch( e->kind )
    {
    // Arith
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod:
    case Expression::Add:
    case Expression::Sub:
    case Expression::And:
    case Expression::Or:
        arithOp(e);
        break;
    // Relation
    case Expression::Eq:
    case Expression::Neq:
    case Expression::Lt:
    case Expression::Leq:
    case Expression::Gt:
    case Expression::Geq:
        relOp(e);
       break;
    case Expression::In:
        inOp(e);
        break;
    case Expression::Is:
        isOp(e);
        break;
    }
}

void Validator::arithOp(Expression* e)
{
    Type* lhsT = deref(e->lhs->type);
    Type* rhsT = deref(e->rhs->type);
    e->type = lhsT;
    if( lhsT->isNumber() && rhsT->isNumber() )
    {
        if( lhsT->isInteger() && rhsT->isInteger() )
            switch(e->kind)
            {
            case Expression::Mul:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toLongLong() * e->rhs->val.toLongLong();
                    toConstVal(e);
                }
                break;
            case Expression::Div:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toLongLong() / e->rhs->val.toLongLong();
                    toConstVal(e);
                }
                break;
            case Expression::Mod:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toLongLong() % e->rhs->val.toLongLong();
                    toConstVal(e);
                }
                break;
            case Expression::Add:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toLongLong() + e->rhs->val.toLongLong();
                    toConstVal(e);
                }
                break;
            case Expression::Sub:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toLongLong() - e->rhs->val.toLongLong();
                    toConstVal(e);
                }
                break;
            default:
                error(e->pos,"operator not supported for integer operands");
                break;
            }
        else if( lhsT->isReal() && rhsT->isReal() )
            switch(e->kind)
            {
            case Expression::Mul:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toDouble() * e->rhs->val.toDouble();
                    toConstVal(e);
                }
                break;
            case Expression::Fdiv:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toDouble() / e->rhs->val.toDouble();
                    toConstVal(e);
                }
                break;
            case Expression::Add:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toDouble() + e->rhs->val.toDouble();
                    toConstVal(e);
                }
                break;
            case Expression::Sub:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toDouble() - e->rhs->val.toDouble();
                    toConstVal(e);
                }
                break;
            default:
                error(e->pos,"operator not supported for real operands");
                break;
            }
        else
            error(e->pos,"operands are not of the same type");
    }else if( lhsT->isSet() && rhsT->isSet() )
    {
        switch(e->kind)
        {
        case Expression::Mul:
            if( e->isConst() )
            {
                e->val = e->lhs->val.toUInt() & e->rhs->val.toUInt();
                toConstVal(e);
            }
            break;
        case Expression::Fdiv:
            if( e->isConst() )
            {
                e->val = ~(e->lhs->val.toUInt() & e->rhs->val.toUInt()) & (e->lhs->val.toUInt() | e->rhs->val.toUInt());
                toConstVal(e);
            }
            break;
        case Expression::Add:
            if( e->isConst() )
            {
                e->val = e->lhs->val.toUInt() | e->rhs->val.toUInt();
                toConstVal(e);
            }
            break;
        case Expression::Sub:
            if( e->isConst() )
            {
                e->val = e->lhs->val.toUInt() & ~e->rhs->val.toUInt();
                toConstVal(e);
            }
            break;
        default:
            error(e->pos,"operator not supported for set operands");
            break;
        }
    }else if(lhsT->isBoolean() && rhsT->isBoolean())
    {
        switch(e->kind)
        {
        case Expression::And:
            if( e->isConst() )
            {
                e->val = e->lhs->val.toBool() && e->rhs->val.toBool();
                toConstVal(e);
            }
            break;
        case Expression::Or:
            if( e->isConst() )
            {
                e->val = e->lhs->val.toBool() || e->rhs->val.toBool();
                toConstVal(e);
            }
            break;
        default:
            error(e->pos,"operator not supported for boolean operands");
            break;
        }
    }else if((lhsT->form == BasicType::StrLit || lhsT->form == BasicType::CHAR || lhsT->form == BasicType::STRING) &&
             (rhsT->form == BasicType::StrLit || rhsT->form == BasicType::CHAR || rhsT->form == BasicType::STRING ))
    {
        if( e->kind != Expression::Add )
            error(e->pos,"only the '+' operator can be applied to string and char literals");
        else if( e->isConst() )
        {
            QByteArray lhs;
            QByteArray rhs;
            if( lhsT->form == BasicType::CHAR)
                lhs = QByteArray(1,(char)e->lhs->val.toUInt());
            else
                lhs = e->lhs->val.toByteArray();
            if( rhsT->form == BasicType::CHAR)
                rhs = QByteArray(1,(char)e->rhs->val.toUInt());
            else
                rhs = e->rhs->val.toByteArray();
            e->val = lhs + rhs;
            toConstVal(e);
            e->type = mdl->getType(BasicType::StrLit);
        }else
            e->type = mdl->getType(BasicType::STRING);
        // NOTE: we support string concat ops at runtime; string literals are just of type STRING at runtime
    }else
        error(e->pos,"operands not compatible with operator");
}

static bool intRelOp(Expression* e, qint64 l, qint64 r, bool isConst)
{
    switch(e->kind)
    {
    case Expression::Geq:
        if( isConst )
            e->val = l >= r;
        break;
    case Expression::Gt:
        if( isConst )
            e->val = l > r;
        break;
    case Expression::Eq:
        if( isConst )
            e->val = l == r;
        break;
    case Expression::Leq:
        if( isConst )
            e->val = l <= r;
        break;
    case Expression::Neq:
        if( isConst )
            e->val = l != r;
        break;
    case Expression::Lt:
        if( isConst )
            e->val = l < r;
        break;
    default:
        return false;
    }
    if( isConst )
        toConstVal(e);
    return true;
}

void Validator::relOp(Expression* e)
{
    e->type = mdl->getType(BasicType::BOOLEAN);
    Type* lhsT = deref(e->lhs->type);
    Type* rhsT = deref(e->rhs->type);
    if( lhsT->isNumber() && rhsT->isNumber() )
    {
        if( lhsT->isInteger() && rhsT->isInteger() )
        {
            if( !intRelOp(e, e->lhs->val.toLongLong(), e->rhs->val.toLongLong(), e->isConst() ) )
                error(e->pos,"operator not supported for integer operands");
        }else if(lhsT->isReal() && rhsT->isReal() )
        {
            switch(e->kind)
            {
            case Expression::Geq:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toDouble() >= e->rhs->val.toDouble();
                    toConstVal(e);
                }
                break;
            case Expression::Gt:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toDouble() > e->rhs->val.toDouble();
                    toConstVal(e);
                }
                break;
            case Expression::Eq:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toDouble() == e->rhs->val.toDouble();
                    toConstVal(e);
                }
                break;
            case Expression::Leq:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toDouble() <= e->rhs->val.toDouble();
                    toConstVal(e);
                }
                break;
            case Expression::Neq:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toDouble() != e->rhs->val.toDouble();
                    toConstVal(e);
                }
                break;
            case Expression::Lt:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toDouble() < e->rhs->val.toDouble();
                    toConstVal(e);
                }
                break;
            default:
                error(e->pos,"operator not supported for real operands");
                break;
            }
        }else
            error(e->pos, "operands are not of the same type");
    }else if( lhsT->isReference() && rhsT->form == BasicType::Nil ||
              lhsT->form == BasicType::Nil && rhsT->isReference() ||
              lhsT->form == BasicType::Nil && rhsT->form == BasicType::Nil ||
              lhsT->isReference() && rhsT->isReference() ||
              lhsT->isSet() && rhsT->isSet() ||
              lhsT->isBoolean() && rhsT->isBoolean() )
    {
        switch(e->kind)
        {
        case Expression::Eq:
            if( e->isConst() )
            {
                e->val = e->lhs->val.toULongLong() == e->rhs->val.toULongLong();
                toConstVal(e);
            }
            break;
        case Expression::Neq:
            if( e->isConst() )
            {
                e->val = e->lhs->val.toULongLong() != e->rhs->val.toULongLong();
                toConstVal(e);
            }
            break;
        default:
            error(e->pos,"operator not supported for these operands");
            break;
        }
    }else if( lhsT == rhsT && lhsT->form == Type::ConstEnum )
    {
        qint64 l = 0, r = 0;
        const bool isConst = e->isConst();
        if( isConst )
        {
            Q_ASSERT( e->lhs->kind == Expression::DeclRef && e->rhs->kind == Expression::DeclRef );
            Declaration* dl = e->lhs->val.value<Declaration*>();
            Declaration* dr = e->rhs->val.value<Declaration*>();
            Q_ASSERT(dl->kind == Declaration::ConstDecl && dl->mode == Declaration::Enum );
            Q_ASSERT(dr->kind == Declaration::ConstDecl && dr->mode == Declaration::Enum );
            l = dl->data.toLongLong();
            r = dr->data.toLongLong();
        }
        if( !intRelOp(e, l, r, isConst) )
            error(e->pos,"operator not supported for enumeration operands");
    }else if( (lhsT->form == BasicType::CHAR || e->lhs->isCharLiteral()) &&
              (rhsT->form == BasicType::CHAR || e->rhs->isCharLiteral()) )
    {
        quint8 l = 0;
        quint8 r = 0;
        const bool isConst = e->isConst();
        if( isConst )
        {
            if( lhsT->form == BasicType::CHAR )
                l = e->lhs->val.toUInt();
            else
                l = (quint8)e->lhs->val.toByteArray()[0];
            if( rhsT->form == BasicType::CHAR )
                r = e->rhs->val.toUInt();
            else
                r = (quint8)e->rhs->val.toByteArray()[0];
        }

        if( !intRelOp(e, l, r, isConst) )
            error(e->pos,"operator not supported for char operands");
    }else if( (lhsT->form == BasicType::StrLit || lhsT->form == BasicType::STRING) &&
             (rhsT->form == BasicType::StrLit || rhsT->form == BasicType::STRING) )
    {
        // comparison of char arrays not supported; TODO: maybe adding a STRCMP builtin for char arrays?
        QString l, r;
        const bool isConst = e->isConst();
        if( isConst )
        {
            l = QString::fromLatin1(e->lhs->val.toByteArray());
            r = QString::fromLatin1(e->rhs->val.toByteArray());
        }
        switch(e->kind)
        {
        case Expression::Geq:
            if( isConst )
                e->val = l >= r;
            break;
        case Expression::Gt:
            if( isConst )
                e->val = l > r;
            break;
        case Expression::Eq:
            if( isConst )
                e->val = l == r;
            break;
        case Expression::Leq:
            if( isConst )
                e->val = l <= r;
            break;
        case Expression::Neq:
            if( isConst )
                e->val = l != r;
            break;
        case Expression::Lt:
            if( isConst )
                e->val = l < r;
            break;
        default:
            error(e->pos,"operator not supported for string operands");
        }
        if( isConst )
            toConstVal(e);
    }else if( lhsT->form == Type::Generic && rhsT->form == Type::Generic && lhsT == rhsT )
    {
        switch(e->kind)
        {
        case Expression::Eq:
        case Expression::Neq:
            break;
        default:
            error(e->pos,"operator not supported for generic operands");
        }
    }else
        error(e->pos, "operands not compatible with operator");
}

void Validator::assigOp(Statement* s)
{
    if( s->lhs == 0 || s->rhs == 0 )
        return;
    visitExpr(s->lhs);
    visitExpr(s->rhs, s->lhs->type);
    if( !assigCompat(s->lhs->type, s->rhs) ) {
        //assigCompat(s->lhs->type, s->rhs); // TEST
        error(s->pos,"left side not assignment compatible with right side");
    }
}

Statement* Validator::ifOp(Statement* s)
{
    visitExpr(s->rhs);
    if( s->rhs == 0 || s->rhs->type == 0 )
        return s;
    if( !deref(s->rhs->type)->isBoolean() )
        error(s->rhs->pos, "expecting boolean expression");
    visitBody(s->body);
    while( s->getNext() && s->getNext()->kind == Statement::Elsif )
    {
        s = s->getNext();
        visitExpr(s->rhs);
        visitBody(s->body);
    }
    if( s->getNext() && s->getNext()->kind == Statement::Else )
    {
        s = s->getNext();
        visitBody(s->body);
    }
    return s;
}

static QList<qint64> getNumbers(Expression* e, bool&  ok)
{
    QList<qint64> res;
    if( e->kind == Expression::Range )
    {
        const qint64 a = e->lhs->getCaseValue(&ok);
        if( !ok )
            return res;
        const qint64 b = e->rhs->getCaseValue(&ok);
        if( !ok )
            return res;
        if( a <= b )
            for( int i = a; i <= b; i++ )
                res << i;
        else
            for( int i = b; i <= a; i++ )
                res << i;
    }else
    {
        res << e->getCaseValue(&ok);
    }
    return res;
}

Statement*Validator::caseStat(Statement* s)
{
    visitExpr(s->rhs);
    Expression* caseExp = s->rhs;
    Type* caseExpT = deref(caseExp->type);
    QSet<qint64> labels;
    bool typecase = false;
    bool first = true;
    while(s->getNext() && s->getNext()->kind == Statement::CaseLabel )
    {
        s = s->getNext();
        visitExpr(s->rhs);
        if( s->rhs->val.canConvert<Declaration*>() && s->rhs->val.value<Declaration*>()->kind == Declaration::TypeDecl )
        {
            // labels for type case
            if( first )
            {
                typecase = true;
                s->kind = Statement::TypeCase;
                if( caseExpT->form != Type::Record )
                {
                    error(caseExp->pos, "type case only supported for record types");
                    return s;
                }else if( !caseExp->isLvalue() )
                {
                    error(caseExp->pos, "type case expression must be a variable, parameter or field designator");
                    return s;
                }
            }
            if( !typecase )
                error(s->pos, "expecting integer, enumeration or char type label");
            else if( s->rhs->next )
                error(s->rhs->next->pos,"only one case label supported per type case");
            else if( !Type::isSubtype(caseExpT,deref(s->rhs->type)) )
                error(s->rhs->pos,"case label type must be an extension of the case expression type");
        }else
        {
            // labels for value case
            Expression* l = s->rhs;
            while( l )
            {
                if( !first && typecase )
                    error(s->pos, "expecting a type declaration name");
                else if( !l->isConst() )
                    error(l->pos,"expecting constant expression");
                else
                {
                    bool ok;
                    QList<qint64> n = getNumbers(l, ok);
                    if( !ok )
                    {
                       // getNumbers(l, ok); // TEST
                        error(l->pos, "expecting integer, enumeration or char type label");
                    }
                    else if( !assigCompat(caseExp->type, l->kind == Expression::Range ? l->lhs : l ) )
                        error(l->pos,"label type is not compatible with case expression");
                    else
                    {
                        foreach( qint64 v, n )
                        {
                            if( labels.contains(v) )
                            {
                                error(l->pos, "duplicate labels");
                                break;
                            }
                            labels.insert(v);
                        }
                    }
                }
                l = l->next;
            }
        }
        if( typecase )
        {
            Declaration* d = caseExp->val.value<Declaration*>();
            Q_ASSERT(d);
            Type* old = d->type;
            d->type = s->rhs->type;
            visitBody(s->body);
            d->type = old;
        }else
            visitBody(s->body);
        first = false;
    }
    if( s->getNext() && s->getNext()->kind == Statement::Else )
    {
        s = s->getNext();
        visitBody(s->body);
    }
    return s;
}

Statement*Validator::forStat(Statement* s)
{
    // TODO: support enums?
    visitExpr(s->lhs);
    visitExpr(s->rhs);
    if( s->lhs->type == 0 || s->rhs->type == 0 )
        return s;
    if( !deref(s->lhs->type)->isInteger() || !s->lhs->isLvalue() )
        error(s->lhs->pos, "expecting an integer variable");
    if( !deref(s->rhs->type)->isInteger() )
        error(s->rhs->pos, "expecting an integer expression");
    visitBody(s->body);
    if( s->getNext() && s->getNext()->kind == Statement::ForToBy )
    {
        s = s->getNext();
        visitExpr(s->lhs);
        visitExpr(s->rhs);
        if( s->lhs->type == 0 )
            return s;
        if( !deref(s->lhs->type)->isInteger() )
            error(s->lhs->pos, "expecting an integer expression");
        if( s->rhs )
        {
            if( !s->rhs->isConst() || !deref(s->rhs->type)->isInteger() )
                error(s->lhs->pos, "expecting a constant integer expression");
        }
    }else
        Q_ASSERT(false);
    return s;
}

void Validator::loopStat(Statement* s)
{
    visitExpr( s->rhs );
    if( s->rhs == 0 || s->rhs->type == 0 )
        return;
    if( !deref(s->rhs->type)->isBoolean() )
        error(s->rhs->pos, "expecting boolean expression");
    visitBody(s->body);
}

void Validator::returnOp(Statement* s)
{
    visitExpr(s->rhs);
    Q_ASSERT(!scopeStack.isEmpty() && scopeStack.back()->kind == Declaration::Procedure);
    if( s->rhs && (scopeStack.back()->type == 0 || scopeStack.back()->type->form == BasicType::NoType) )
        error(s->rhs->pos,"the procedure doesn't return a value");
    else if( s->rhs == 0 && scopeStack.back()->type != 0 && scopeStack.back()->type->form != BasicType::NoType)
        error(s->pos,"a value must be returned");
    else if( s->rhs != 0 && scopeStack.back()->type != 0 && scopeStack.back()->type->form != BasicType::NoType
             && !assigCompat(scopeStack.back()->type, s->rhs) )
        error(s->rhs->pos,"the returned value is not compatible with the function return type");
}

Qualident Validator::resolve(Expression* nameRef)
{
    Q_ASSERT( !scopeStack.isEmpty() );

    Qualident q = nameRef->val.value<Qualident>();

    ResolvedQ r;
    QByteArray select;
    if( !q.first.isEmpty() )
    {
        // check whether this is a local name or an import
        Declaration* d = scopeStack.back()->find(q.first);
        if( d == 0 )
        {
            error(nameRef->pos,QString("declaration '%1' not found").arg(q.first.constData()));
            return Qualident();
        }else if( d->kind == Declaration::Import )
        {
            r = find(q,nameRef->pos, d);
        }else
        {
            // this is a local select, not an import
            select = q.second;
            q.second = q.first;
            q.first.clear();
            r.second = d;
            nameRef->val = QVariant::fromValue(q);
        }
    }else
        r = find(q,nameRef->pos);
    if( r.second == 0 )
        return Qualident();

    RowCol pos = nameRef->pos;
    if( r.first != 0 )
    {
        markRef(r.first, pos);
        pos.d_col += q.first.size() + 1;
    }
    Symbol* s = markRef(r.second, pos);
    if( nameRef->needsLval )
        s->kind = Symbol::Lval;
    resolve(r.second->type);
#if 0
    visitDecl(d); // RISK: we need that so that procedures are resolved when passing as meta actuals
    // does not work yet because it triggers circular dependencies
#endif

    // repurpose the expression
    if( r.second->kind == Declaration::ConstDecl && r.second->mode != Declaration::Enum )
    {
        nameRef->val = r.second->data;
        nameRef->type = r.second->type;
        toConstVal(nameRef);
    }else
    {
        // Enum ConstDecls are handled here because they are used as symbols in the first place
        toConstVal(nameRef);
        nameRef->kind = Expression::DeclRef;
        nameRef->val = QVariant::fromValue(r.second);
        nameRef->type = r.second->type;
        if( r.second->kind == Declaration::LocalDecl || r.second->kind == Declaration::ParamDecl )
        {
            if( r.second->outer != scopeStack.back() )
                error(nameRef->pos,"cannot access parameters and local variables of outer procedures");
            // Luon - in contrast to Oberon+ - doesn't support non-local variable access
        }
    }
    return Qualident(!select.isEmpty() ? q.second : QByteArray(),select);
}

Validator::ResolvedQ Validator::find(const Qualident& q, const RowCol& pos, Declaration* import)
{
    if( scopeStack.isEmpty() )
        return ResolvedQ();

    ResolvedQ res;
    if( !q.first.isEmpty() )
    {
        if( import == 0 )
            import = scopeStack.back()->find(q.first);
        if( import == 0 || import->kind != Declaration::Import )
        {
            error(pos,"identifier doesn't refer to an imported module");
            return ResolvedQ();
        }
        if( !import->validated )
            visitImport(import);
        res.first = import;
        Declaration* member = mdl->findDecl(import,q.second);
        if( member == 0 )
        {
            error(pos,QString("declaration '%1' not found in imported module '%2'").
                  arg(q.second.constData()).arg(q.first.constData()) );
        }else
        {
            if( member->visi == Declaration::Private )
                error(pos,QString("cannot access private declaration '%1' from module '%2'").
                      arg(q.second.constData()).arg(q.first.constData()) );
            res.second = member;
        }
    }else
    {
        Declaration* d = scopeStack.back()->find(q.second);
        if( d == 0 )
            d = mdl->findDecl(q.second);
        if( d == 0 )
            error(pos,QString("declaration '%1' not found").arg(q.second.constData()));
        res.second = d;
    }
    return res;
}

void Validator::selectOp(Expression* e)
{
    if( e->lhs == 0 || e->lhs->type == 0 )
        return;
    Type* lhsT = deref(e->lhs->type);
    if( lhsT->form == Type::Record )
    {
        Declaration* ld = e->lhs->val.value<Declaration*>();
        if( ld && ld->kind == Declaration::TypeDecl )
            error(e->lhs->pos,"selector expects a variable on the left side");
        Declaration* field = lhsT->find(e->val.toByteArray());
        if( field == 0 )
            error(e->pos,QString("the record doesn't have a field named '%1'"). arg(e->val.toString()) );
        else
        {
            Symbol* s = markRef(field, e->pos);
            if( e->needsLval )
                s->kind = Symbol::Lval;
            e->val = QVariant::fromValue(field); // Field or bound proc
            e->type = field->type;
        }
    }else
        error(e->pos,"cannot select a field in given type");
}

void Validator::callOp(Expression* e)
{
    bool supercall = false;
    Expression* lhs = e->lhs;
    if( lhs && lhs->kind == Expression::Super )
    {
        supercall = true;
        lhs = lhs->lhs;
    }
    if( lhs == 0 || lhs->type == 0 ) // e->rhs is null in case there are no args
        return;

    Declaration* proc = lhs->val.value<Declaration*>();
    if( proc && proc->kind != Declaration::Procedure && proc->kind != Declaration::Builtin )
        proc = 0;
    Type* procType = deref(lhs->type);
    if( procType && procType->form != Type::Proc )
        procType = 0;

    const DeclList formals = proc ? proc->getParams(true) : procType ? procType->subs : DeclList();
    Expression* arg = e->rhs;
    for(int i = 0; arg != 0; i++, arg = arg->next )
    {
        Type* hint = i < formals.size() ? formals[i]->type : 0;
        visitExpr(arg, hint, false);
    }

    const bool isTypeCast = (proc == 0 || proc->kind != Declaration::Builtin) &&
            e->rhs &&
            e->rhs->kind == Expression::DeclRef &&
            e->rhs->val.value<Declaration*>()->kind == Declaration::TypeDecl;

    Type* lhsT = deref(lhs->type);
    if( isTypeCast )
    {
        if( supercall )
            return error(e->pos,"super call operator cannot be used here");
        e->kind = Expression::Cast;
        e->type = deref(e->rhs->type);
        if( e->rhs->next )
            return error(e->rhs->next->pos,"type guard requires a single argument");
        if( lhsT->form != Type::Record )
            return error(e->rhs->pos,"a type guard is not supported for this type");
    }else
    {
        if( proc )
        {
            if( proc->kind != Declaration::Procedure && proc->kind != Declaration::Builtin )
                return error(lhs->pos,"this expression cannot be called");
            e->type = proc->type;
        }else if( lhsT->form != Type::Proc )
            return error(lhs->pos,"this expression cannot be called");
        else
            e->type = lhsT->base;

        if( supercall && (proc == 0 || proc->mode != Declaration::Receiver || proc->super == 0) )
            return error(e->pos,"super call operator cannot be used here");

        ExpList actuals = Expression::getList(e->rhs);

        if( proc && proc->kind == Declaration::Builtin )
        {
            if( proc->id == Builtin::ASSERT )
            {
                Expression* ee = new Expression(Expression::Literal,e->pos);
                ee->type = mdl->getType(BasicType::INTEGER);
                ee->val = lhs->pos.d_row;
                e->appendRhs(ee);
                ee = new Expression(Expression::Literal,e->pos);
                ee->type = mdl->getType(BasicType::StrLit);
                ModuleData md = module->data.value<ModuleData>();
                ee->val = md.fullName;
                e->appendRhs(ee);
            }
            if( checkBuiltinArgs(proc->id, actuals, &e->type, e->pos) )
            {
                QVariant val;
                if( evalBuiltin(proc->id, actuals, val, e->pos ) )
                {
                    e->val = val;
                    toConstVal(e);
                }
            }
        }else
        {
            if( actuals.size() != formals.size() )
                error(e->pos,"number of actual doesn't fit number of formal arguments");

            for( int i = 0; i < formals.size() && i < actuals.size(); i++ )
            {
                if( !paramCompat(formals[i],actuals[i]) )
                {
                    paramCompat(formals[i],actuals[i]); // TEST
                    error(actuals[i]->pos, "actual argument not compatible with formal parameter");
                }
            }
            // TODO: compiletime execution if invar
        }
    }
}

void Validator::callOp(Statement* s)
{
    Q_ASSERT( s->lhs );
    if( s->lhs->kind != Expression::Call )
    {
        Expression* call = new Expression(Expression::Call, s->lhs->pos);
        call->lhs = s->lhs;
        s->lhs = call;
    }
    visitExpr(s->lhs);
}

void Validator::indexOp(Expression* e)
{
    if( e->lhs == 0 || e->lhs->type == 0 || e->rhs == 0 ||e->rhs->type == 0 )
        return;

    Type* lhsT = deref(e->lhs->type);
    Type* rhsT = deref(e->rhs->type);

    e->type = lhsT->base;
    if( lhsT->form == Type::Array )
    {
        if( !rhsT->isInteger() )
            error(e->rhs->pos,"expecting an array index of integer type");
    }else if(lhsT->form == Type::HashMap)
    {
        if( !assigCompat(lhsT->expr->type, rhsT ) )
            error(e->rhs->pos,"expression not compatible with key type");
    }else
        error(e->pos,"cannot index an element in given type");
}

void Validator::inOp(Expression* e)
{
    if( e->lhs == 0 || e->lhs->type == 0 || e->rhs == 0 || e->rhs->type == 0 )
        return;
    Type* lhsT = deref(e->lhs->type);
    Type* rhsT = deref(e->rhs->type);
    e->type = mdl->getType(BasicType::BOOLEAN);
    if( !lhsT->isInteger() )
        return error(e->lhs->pos,"expecting integer type");
    if( !rhsT->isSet() )
        return error(e->rhs->pos,"expecting set type");
    if( e->isConst() )
    {
        std::bitset<32> bits(e->rhs->val.toUInt());
        qint64 n = e->lhs->val.toLongLong();
        if( n < 0 || n > 31 )
            return error(e->lhs->pos,"index out of range");
        e->val = bits.test(n);
        toConstVal(e);
    }
}

void Validator::isOp(Expression* e)
{
    if( e->lhs == 0 || e->lhs->type == 0 || e->rhs == 0 ||e->rhs->type == 0 )
        return;
    Type* lhsT = deref(e->lhs->type);
    Type* rhsT = deref(e->rhs->type);
    e->type = mdl->getType(BasicType::BOOLEAN);
    if( lhsT->form != Type::Record )
        return error(e->lhs->pos,"expecting record type");
    if( rhsT->form != Type::Record )
        return error(e->rhs->pos,"expecting record type");
    if( e->rhs->kind != Expression::DeclRef || e->rhs->val.value<Declaration*>()->kind != Declaration::TypeDecl )
        return error(e->rhs->pos,"expecting a type declaration");
    // TODO: ANYREC
    if( !Type::isSubtype(lhsT, rhsT) )
        return error(e->rhs->pos,"rhs type must be an extension of lhs type");
}

static inline int find_( const QList<Declaration*>& l, const QByteArray& name )
{
    for( int i = l.size() - 1; i >= 0; i-- )
    {
        if( l[i]->name.constData() == name.constData() )
            return i;
    }
    return -1;
}

void Validator::constructor(Expression* constr, Type* hint)
{
    // e->type is preset if NamedType is present
    // otherwise we use hint, or SET
    if( constr->type == 0 )
    {
        if( hint )
            constr->type = deref(hint);
        else
            constr->type = mdl->getType(BasicType::SET);
    }
    QList<Declaration*> fields;
    if( constr->type->form == Type::Record)
        fields = constr->type->fieldList();
    int index = 0;
    int maxIndex = 0;
    bool allConst = true;
    Type* constrT = deref(constr->type);
    // e is of the structure { c, c, c, .. }
    Expression* comp = constr->rhs;
    while( comp )
    {
        // go through all components
        if( constrT->form == Type::Record )
        {
            if( comp->kind == Expression::KeyValue )
            {
                if( !comp->byName )
                    return error(comp->pos,"index components not supported in record constructors");
                Q_ASSERT( comp->lhs->kind == Expression::NameRef );
                Qualident q = comp->lhs->val.value<Qualident>();
                index = find_(fields, q.second);
                if( index < 0 )
                    return error(comp->pos,"field not known in record or its base records");
                visitExpr(comp->rhs, fields[index]->type);
                comp->lhs->val = QVariant::fromValue(fields[index]);
                comp->type = comp->rhs->type;
                if( comp->rhs->kind == Expression::Range )
                    return error(comp->pos,"cannot use a range here");
            }else
            {
                if( comp->kind == Expression::Range )
                    return error(comp->pos,"cannot use a range here");
                if( index >=  fields.size() )
                    return error(comp->pos,"component position out of record");
                visitExpr(comp, fields[index]->type, false);
            }
            if( !assigCompat(fields[index]->type,comp->type) )
                return error(comp->pos,"component type incompatible with field type");
        }else if( constrT->form == Type::Array )
        {
            if( comp->kind == Expression::KeyValue )
            {
                if( comp->byName )
                    return error(comp->pos,"label components not supported in array constructors");
                visitExpr(comp->lhs);
                if( !comp->lhs->isConst() || deref(constr->lhs->type)->isInteger() )
                    return error(comp->pos,"index must be a constant expression of integer type");
                index = comp->lhs->val.toLongLong();
                visitExpr(comp->rhs, constrT->base);
                comp->type = comp->rhs->type;
                if( comp->rhs->kind == Expression::Range )
                    return error(comp->pos,"cannot use a range here");
            }else
            {
                if( comp->kind == Expression::Range )
                    return error(comp->pos,"cannot use a range here");
                visitExpr(comp, constrT->base, false);
            }
            if( constrT->len && index >= constrT->len )
                return error(comp->pos,"component position out of array");
            if( !assigCompat(constrT->base,comp->type) )
                return error(comp->pos,"component type incompatible with element type");
        }else if( constrT->form == Type::HashMap )
        {
            // TODO: a : b could be the same as ["a"] : b, but then we should also support h.a for h["a"]
            if( comp->kind == Expression::KeyValue )
            {
                if( comp->byName )
                    return error(comp->pos,"label components not supported in record constructors");
                visitExpr(comp->lhs);
                if( constrT->expr && !assigCompat(constrT->expr->type, comp->lhs->type) )
                    return error(comp->pos,"index type of component not compatible with key type");
                visitExpr(comp->rhs, constrT->base);
                comp->type = comp->rhs->type;
                if( comp->rhs->kind == Expression::Range )
                    return error(comp->pos,"cannot use a range here");
            }else
                return error(comp->pos,"only key-value-components supported for HASHMAP constructors");
        }else if( constrT->form == BasicType::SET )
        {
            if( comp->kind == Expression::KeyValue || comp->kind == Expression::Constructor )
                return error(comp->pos,"component type not supported for SET constructors");
            visitExpr(comp, 0, false);
            if( comp->kind == Expression::Range && !sameType(comp->lhs->type, comp->rhs->type) )
                error(constr->pos,"types in range must be the same");
            if( comp->type && !deref(comp->type)->isInteger() )
                return error(comp->pos,"expecting integer compontents for SET constructors");
        }else
        {
            return error(constr->pos,"constructors cannot be used to create the given type");
            // stop immediately
        }
        if( !comp->isConst() )
            allConst = false;
        if( index > maxIndex )
            maxIndex = index;
        index++;
        comp = comp->next;
    }
    if( constrT->form == Type::Array && constrT->len == 0 )
    {
        // create new array type with fix len = maxIndex+1
        Type* a = new Type();
        a->form = Type::Array;
        a->len = maxIndex + 1;
        a->base = deref(constrT->base);
        a->anonymous = true;
        addHelper(a);
        constr->type = a;
    }else if( constrT->form == BasicType::SET && allConst )
    {
        Expression* c = constr->rhs;
        std::bitset<32> bits;
        while( c )
        {
            Q_ASSERT( c->isConst() );
            if( c->kind == Expression::Range )
            {
                const quint64 a = c->lhs->val.toLongLong();
                const quint64 b = c->rhs->val.toLongLong();
                if( a < 0 || a > 31 || b < 0 || b > 31 )
                    return error(c->pos,"set component out of range");
                if( a <= b )
                    for(int i = a; i <= b; i++ )
                        bits.set(i);
                else
                    for(int i = b; i <= a; i++ )
                        bits.set(i);
            }else
            {
                const quint64 n = c->val.toLongLong();
                if( n < 0 || n > 31 )
                    error(c->pos,"set component out of range");
                else
                    bits.set(n);
            }
            c = c->next;
        }
        toConstVal(constr);
        constr->val = (quint64)bits.to_ulong();
    }
}

Type*Validator::deref(Type* t) const
{
    // never returns zero
    if( t == 0 )
        return mdl->getType(BasicType::NoType);
    if( t->form == Type::NameRef )
    {
        if( !t->validated )
        {
            Qualident q = t->decl->data.value<Qualident>();
            QString name = q.second;
            if( !q.first.isEmpty() )
                name = QString(q.first) + "." + name;
            Declaration* m = t->decl->getModule();
            ModuleData md = m->data.value<ModuleData>();
            errors << Error(QString("cannot resolve %1").arg(name), t->decl->pos, md.source);
            t->validated = true; // avoid duplicate errors
        }
        return t->deref();
    }else
        return t;
}

bool Validator::assigCompat(Type* lhs, Type* rhs) const
{
    lhs = deref(lhs);
    rhs = deref(rhs);

    if( sameType(lhs,rhs) )
        return true;

    if( lhs->form == Type::Array && rhs->form == Type::Array && (lhs->len == 0 || lhs->len == rhs->len) &&
            equalTypes(lhs->base, rhs->base) )
        return true;

    if( lhs->form == BasicType::STRING && rhs->isDerefCharArray() )
        // this is an abbreviation of COPY(lhs,rhs), i.e. an implicit copy operation
        return true;

    if( lhs->form == BasicType::STRING && rhs->form == BasicType::StrLit )
        return true;

    if( ( lhs->form == Type::Record || lhs->form == Type::Array ||
          lhs->form == Type::HashMap || lhs->form == Type::Proc ) && rhs->form == BasicType::Nil )
        return true;

    if( lhs->form == Type::Record && rhs->form == Type::Record && Type::isSubtype(lhs,rhs) )
        return true;

    if( lhs->form == Type::Proc && rhs->form == Type::Proc )
        return matchFormals(lhs->subs, rhs->subs) && matchResultType(lhs->base,rhs->base);

    if( lhs->form == BasicType::INTEGER && rhs->form == BasicType::BYTE )
        return true;

#if 0
    // no, we should use copy for this
    if( lhs->isDerefCharArray() && rhs->isDerefCharArray())
        return true; // check len at runtime
#endif

    return false;
}

bool Validator::assigCompat(Type* lhs, Declaration* rhs) const
{
    if( rhs == 0 || rhs->kind == Declaration::TypeDecl )
        return false;

    if( lhs == 0 )
        return false;

    lhs = deref(lhs);

    // Tv is a procedure type and e is the name of a procedure whose formal parameters match those of Tv.
    if( lhs->form == Type::Proc && rhs->kind == Declaration::Procedure )
    {
        if( lhs->receiver && rhs->mode != Declaration::Receiver || !lhs->receiver && rhs->mode == Declaration::Receiver )
            return false;
        return matchFormals(lhs->subs, rhs->getParams(true)) && matchResultType(lhs->base,rhs->type);
    }

    // Tv is an enumeration type and e is a valid element of the enumeration;
    if( lhs->form == Type::ConstEnum && rhs->kind == Declaration::ConstDecl && rhs->mode == Declaration::Enum )
        return lhs->subs.contains(rhs);

    return assigCompat(lhs, rhs->type);
}

bool Validator::assigCompat(Type* lhs, const Expression* rhs) const
{
    lhs = deref(lhs);
    if( rhs == 0 )
        return false;

    if( lhs->form == BasicType::BYTE && deref(rhs->type)->form == BasicType::INTEGER && rhs->isConst() )
    {
        int i = rhs->val.toInt();
        return i >= 0 && i <= 255;
    }

    Type* rhsT = deref(rhs->type);
    if( rhs->kind == Expression::DeclRef || rhs->kind == Expression::Select )
        return assigCompat(lhs, rhs->val.value<Declaration*>() );

    // A string of length 1 can be used wherever a character constant is allowed and vice versa.
    if( lhs->form == BasicType::CHAR && rhsT->form == BasicType::StrLit )
        return strlen(rhs->val.toByteArray().constData()) == 1;

    if( lhs->isDerefCharArray() && (rhsT->form == BasicType::StrLit || rhsT->form == BasicType::STRING))
    {
        // this is an abbreviation of COPY(lhs,rhs), i.e. an implicit copy operation
        if( lhs->len && rhsT->form == BasicType::StrLit && rhs->val.toByteArray().size() > lhs->len )
            return false;
        else
            return true; // check len at runtime
    }

    if( lhs->isDerefByteArray() && rhsT->form == BasicType::ByteArrayLit )
    {
        // this is an abbreviation of COPY(lhs,rhs), i.e. an implicit copy operation
        if( lhs->len && rhsT->form == BasicType::ByteArrayLit && rhs->val.toByteArray().size() >= lhs->len )
            return false;
        else
            return true; // check len at runtime
    }

    return assigCompat(lhs, rhs->type);
}

bool Validator::paramCompat(Declaration* lhs, const Expression* rhs) const
{
    Q_ASSERT(lhs->kind == Declaration::ParamDecl);

    Type* lhsT = deref(lhs->type);
    Type* rhsT = deref(rhs->type);
    // a string literal (which is a STRING actually) is compatible with an const array of char formal param
    if( lhs->visi == Declaration::ReadOnly && lhsT->isDerefCharArray() && lhsT->len == 0 &&
            (rhsT->form == BasicType::StrLit || rhsT->form == BasicType::STRING ) )
        return true;
    // a byte array literal is compatible with a const array of byte formal param
    if( lhs->visi == Declaration::ReadOnly && lhsT->isDerefByteArray() && lhsT->len == 0 &&
            rhsT->form == BasicType::ByteArrayLit )
        return true;

    if( lhs->varParam && !rhs->isLvalue() )
        return false;

    // Tf and Ta are equal types, or Ta is assignment compatible with Tf
    return equalTypes(lhsT,rhsT) || assigCompat(lhsT,rhs);
}

bool Validator::matchFormals(const QList<Declaration*>& a, const QList<Declaration*>& b) const
{
    if( a.size() != b.size() )
        return false;
    for( int i = 0; i < a.size(); i++ )
    {
        if( i == a.size() - 1 && a[i]->name == ".." && a[i]->name != b[i]->name )
            return false;
        if( a[i]->visi != b[i]->visi )
            return false;
        if( !equalTypes(a[i]->type, b[i]->type) )
            return false;
    }
    return true;
}

bool Validator::matchResultType(Type* lhs, Type* rhs) const
{
    return sameType(lhs,rhs);
}

bool Validator::sameType(Type* lhs, Type* rhs) const
{
    lhs = deref(lhs);
    rhs = deref(rhs);
    return lhs == rhs;
}

bool Validator::equalTypes(Type* lhs, Type* rhs) const
{
    // Ta and Tb are the same type,
    if(sameType(lhs,rhs))
        return true;

    lhs = deref(lhs);
    rhs = deref(rhs);

    // Ta and Tb are open array types with equal element types, or
    // Ta and Tb are non-open array types with same length and equal element types, or
    if( lhs->form == Type::Array && rhs->form == Type::Array && (lhs->len == 0 || lhs->len == rhs->len) &&
            equalTypes(lhs->base, rhs->base) )
        return true;

    if( lhs->form == Type::HashMap && rhs->form == Type::HashMap && lhs->expr && rhs->expr &&
            equalTypes(lhs->expr->type, rhs->expr->type) &&
            equalTypes(lhs->base, rhs->base) )
        return true;

    // Ta and Tb are procedure types whose formal parameters match,
    if( lhs->form == Type::Proc && rhs->form == Type::Proc && matchFormals(lhs->subs,rhs->subs) &&
            matchResultType(lhs->base, rhs->base))
        return true;

    return false;
}

static inline bool expectingNArgs(const ExpList& args,int n)
{
    if( args.size() != n )
        throw QString("expecting %1 arguments").arg(n);
    for( int i = 0; i < args.size(); i++ )
        if( args[i]->type == 0 )
            return false;
    return true;
}

static inline bool expectingNMArgs(const ExpList& args,int n, int m)
{
    if( args.size() < n || args.size() > m)
        throw QString("expecting %1 to %2 arguments").arg(n).arg(m);
    for( int i = 0; i < args.size(); i++ )
        if( args[i]->type == 0 )
            return false;
    return true;
}

static void checkBitArith(const ExpList& args, Type** ret)
{
    if( !expectingNArgs(args,2) )
        return;
    if( !args[0]->type->deref()->isInteger() )
        throw QString("expecing integer first argument");
    if( !args[1]->type->deref()->isInteger() )
        throw QString("expecing integer second argument");
    *ret = args[0]->type;
}

bool Validator::checkBuiltinArgs(quint8 builtin, const ExpList& args, Type** ret, const RowCol& pos)
{
    // NOTE: args are already visited at this point

    Q_ASSERT(ret);

    *ret = mdl->getType(BasicType::NoType);

    Type* t;
    try
    {
    switch(builtin)
    {
    // functions:
    case Builtin::ABS:
        if( !expectingNArgs(args,1) )
            break;
        if( !deref(args.first()->type)->isNumber() )
            throw "expecting numeric argument";
        *ret = args.first()->type;
        break;
    case Builtin::BITAND:
        checkBitArith(args, ret);
        break;
    case Builtin::BITASR:
        checkBitArith(args, ret);
        break;
    case Builtin::BITNOT:
        if( !expectingNArgs(args,1) )
            break;
        if( !deref(args.first()->type)->isInteger() )
            throw "expecting integer";
        *ret = args[0]->type;
        break;
    case Builtin::BITOR:
        checkBitArith(args, ret);
        break;
    case Builtin::BITS:
        expectingNArgs(args,1);
        *ret = mdl->getType(BasicType::SET);
        break;
    case Builtin::BITSHL:
        checkBitArith(args, ret);
        break;
    case Builtin::BITSHR:
        checkBitArith(args, ret);
        break;
    case Builtin::BITXOR:
        checkBitArith(args, ret);
        break;
    case Builtin::CAST:
        if( !expectingNArgs(args,2) )
            break;
        *ret = args[0]->type;
        if( !deref(args.first()->type)->form != Type::ConstEnum )
            throw "expecting enumeration type as a first argument";
        if( !deref(args.last()->type)->isInteger() )
            throw "expecting integer type as a second argument";
       break;
    case Builtin::CHR:
        if( !expectingNArgs(args,1) )
            break;
        *ret = mdl->getType(BasicType::CHAR);
        if( !deref(args.first()->type)->isInteger() )
            throw "expecting integer argument";
        break;
    case Builtin::CLIP:
        if( !expectingNArgs(args,1) )
            break;
        *ret = mdl->getType(BasicType::BYTE);
        if( !deref(args.first()->type)->isInteger() )
            throw "expecting integer argument";
        break;
    case Builtin::DEFAULT:
        if(!expectingNArgs(args,1))
            break;
        t = deref(args[0]->type);
        if( t->isStructured() )
            *ret = mdl->getType(BasicType::Nil);
        else
            *ret = args[0]->type;
        break;    
    case Builtin::FLOOR:
        if( !expectingNArgs(args,1) )
            break;
        if( !deref(args.first()->type)->isReal() )
            throw "expecting real argument";
        *ret = mdl->getType(BasicType::INTEGER);
        break;
    case Builtin::FLT:
        if( !expectingNArgs(args,1) )
            break;
        if( !deref(args.first()->type)->isInteger() )
            throw "expecting integer argument";
        *ret = mdl->getType(BasicType::REAL);
        break;
    case Builtin::LEN:
        if( !expectingNArgs(args,1) )
            break;
        *ret = mdl->getType(BasicType::INTEGER);
        t = deref(args[0]->type);
        if( t->form != Type::Array && t->form != BasicType::STRING
                && t->form != BasicType::StrLit && t->form != BasicType::ByteArrayLit )
            throw "expecing array or string argument";
        break;
    case Builtin::MAX:
    case Builtin::MIN:
        if(!expectingNMArgs(args,1,2))
            break;
        *ret = args[0]->type;
        if( !deref(args[0]->type)->isNumber())
            throw "expecing number argument";
        break;
    case Builtin::ODD:
        if( !expectingNArgs(args,1) )
            break;
        *ret = mdl->getType(BasicType::BOOLEAN);
        if( !deref(args.first()->type)->isInteger() )
            throw "expecting integer argument";
       break;
    case Builtin::ORD:
        if( !expectingNArgs(args,1) )
            break;
        *ret = mdl->getType(BasicType::INTEGER);
        t = deref(args.first()->type);
        if( !t->isSet() && t->form != BasicType::BOOLEAN && t->form != Type::ConstEnum && t->form != BasicType::CHAR)
            throw "expecting set, boolean, enum or char argument";
        break;
    case Builtin::STRLEN:
        if( !expectingNArgs(args,1) )
            break;
        *ret = mdl->getType(BasicType::INTEGER);
        t = deref(args[0]->type);
        if( !t->isDerefCharArray() && t->form != BasicType::STRING && t->form != BasicType::StrLit )
            throw "expecing char array or string argument";
        break;
    case Builtin::ASSERT:
        if( !expectingNArgs(args,1) )
            break; // bool, line, file
        if( deref(args[0]->type)->form != BasicType::BOOLEAN )
            throw "expecting boolean arument";
        // the other two args are set by the compiler
        break;
    case Builtin::INC:
    case Builtin::DEC:
        if( !expectingNMArgs(args,1,2) )
            break;
        if( !args[0]->isLvalue() )
            throw "expecting a variable as the first argument";
        if( !deref(args[0]->type)->isInteger() )
            throw "expecting an integer type first arument";
        if( args.size() == 2 && !deref(args[1]->type)->isInteger() )
            throw "expecting an integer type second arument";
        break;
    case Builtin::TOSTRING:
        if( !expectingNArgs(args,1) )
            break;
        *ret = mdl->getType(BasicType::STRING);
        break;
    case Builtin::TRAPIF:
        if( !expectingNArgs(args,1) )
            break;
        if( deref(args[0]->type)->form != BasicType::BOOLEAN )
            throw "expecting boolean arument";
        break;
    case Builtin::EXCL:
    case Builtin::INCL:
        if( !expectingNArgs(args,2) )
            break;
        if( !args[0]->isLvalue() )
            throw "expecting a variable as the first argument";
        if( !deref(args[0]->type)->isSet() )
            throw "expecting set type first arument";
        if( !deref(args[1]->type)->isInteger() )
            throw "expecting an integer type second arument";
        break;
    case Builtin::NEW: {
        if( !expectingNMArgs(args,1,2) )
            break;
        if( !args[0]->isLvalue() )
            throw "expecting a variable as the first argument";
        Type* t1 = deref(args[0]->type);
        if( t1->form != Type::Record && t1->form != Type::Array && t1->form != Type::HashMap )
            throw "new() expects a structured type first argument";
        if( args.size() == 1 && t1->form == Type::Array && t1->len == 0)
            throw "open arrays require an explicit size argument";
        if( args.size() == 2 && (t1->form != Type::Array || t1->len > 0))
            throw "second argument only applicable to open arrays";
        break;
        }
    case Builtin::COPY:
        // COPY(x): make a (shallow) copy of the array/record/hashmap pointed to by x and return it; if x is
        //          a string or string literal, then create an open char array as a copy of x
        // COPY(x, y): same as x := COPY(y), with the following exceptions:
        // if x and y are char arrays and x is not nil, then the chars of y up to and with the first 0 are copied to x
        // if x is a char array and y is a char literal or string and x is not nil, then the chars of y are copied to x
        if( !expectingNMArgs(args,1,2) )
            break;
        else if( args.size() == 2)
        {
            if( !args[0]->isLvalue() )
                throw "expecting a variable as the first argument";
            Type* lt = deref(args[0]->type);
            Type* rt = deref(args[1]->type);
            if( !lt->isStructured() )
                throw "expecting a structured first argument type";
            if( lt->isDerefCharArray() && rt->isDerefCharArray() )
                break; // both char arrays, do string copy
            if( lt->isDerefCharArray() && ( rt->form == BasicType::StrLit || rt->form == BasicType::STRING))
                break; // copy str literal or STRING to array of char
            if( lt->isDerefByteArray() && rt->form == BasicType::ByteArrayLit )
                break; // copy ba literal to array of byte
            if( !assigCompat(lt, rt) )
                throw "type of second argument is not assignment compatible with first argument";
        }else if(args.size() == 1)
        {
            Type* t = deref(args[0]->type);
            if( !t->isStructured() ) // TODO string/literal
                throw "expecting a record, array or hashmap argument type";
            *ret = args[0]->type;
        }
        break;

        // TODO: check, fix
    case Builtin::PRINT:
        expectingNArgs(args,1);
        break;
    case Builtin::PRINTLN:
        expectingNArgs(args,1);
       break;
    case Builtin::HALT:
        expectingNArgs(args,1);
        break;
    case Builtin::CAP:
        expectingNArgs(args,1);
        break;
    case Builtin::GETENV:
        expectingNArgs(args,2);
        break;
    case Builtin::PCALL:
        break;
    case Builtin::RAISE:
        expectingNArgs(args,1);
        break;
    case Builtin::SETENV:
        expectingNArgs(args,2);
        break;
    }
    }catch( const QString& err )
    {
        error(pos, err);
        return false;
    }catch( const char* str)
    {
        error(pos, str);
        return false;
    }

    return true;
}

bool Validator::evalBuiltin(quint8 builtin, const ExpList& args, QVariant& ret, const RowCol& pos)
{
    // NOTE: args and builtin are already checked at this point

    switch( builtin )
    {
    case Builtin::LEN: {
        // LEN handles compile-time and dynamic arguments
            Type* t = deref(args.first()->type);
            if( t->form == Type::Array )
            {
                if( t->len > 0 )
                {
                    ret = t->len;
                    return true;
                }
            }else if( t->form == BasicType::StrLit )
            {
                ret = args.first()->val.toByteArray().size() + 1; // include terminating zero
                return true;
            }else if( t->form == BasicType::ByteArrayLit )
            {
                ret = args.first()->val.toByteArray().size(); // no terminating zero unless explicitly in literal
                return true;
            }
            return false;
        }
    case Builtin::MAX:
        ret = BasicType::getMax(deref(args.first()->type)->form);
        return true;
    case Builtin::MIN:
        ret = BasicType::getMin(deref(args.first()->type)->form);
        return true;
    case Builtin::DEFAULT:
        switch( deref(args.first()->type)->form )
        {
        case BasicType::BOOLEAN:
            ret = false;
            break;
        case BasicType::CHAR:
        case BasicType::INTEGER:
        case BasicType::BYTE:
        case BasicType::SET:
        case Type::ConstEnum:
            ret = (qint64)0;
            break;
        case BasicType::REAL:
            ret = (double)0.0;
            break;
        default:
            ret = QVariant();
        }
        return true;
    }

    for( int i = 0; i < args.size(); i++ )
        if( !args[i]->isConst() )
            return false;

    switch( builtin )
    {
    case Builtin::ABS:
        if( !deref(args.first()->type)->isInteger() )
            ret = qAbs(args.first()->val.toLongLong());
        else
            ret = qAbs(args.first()->val.toDouble());
        break;
    case Builtin::BITAND:
        ret = args[0]->val.toUInt() & args[1]->val.toUInt(); // LuaJIT only supports 32 bit bitops
        break;
    case Builtin::BITASR:
        ret = args[0]->val.toInt() >> args[1]->val.toUInt();
        break;
    case Builtin::BITNOT:
        ret = ~args[0]->val.toUInt();
        break;
    case Builtin::BITOR:
        ret = args[0]->val.toUInt() | args[1]->val.toUInt();
        break;
    case Builtin::BITS:
        ret = args[0]->val.toUInt();
        break;
    case Builtin::BITSHL:
        ret = args[0]->val.toUInt() << args[1]->val.toUInt();
        break;
    case Builtin::BITSHR:
        ret = args[0]->val.toUInt() >> args[1]->val.toUInt();
        break;
    case Builtin::BITXOR:
        ret = args[0]->val.toUInt() ^ args[1]->val.toUInt();
        break;
    case Builtin::CAST:
        ret = args[0]->val.toUInt(); // TODO: check enums?
        break;
    case Builtin::CHR:
        ret = args[0]->val.toUInt();
        break;
    case Builtin::CLIP:
        ret = args[0]->val.toUInt() & 0xff;
        break;
    case Builtin::FLOOR:
        ret = (qint64)::floor(args[0]->val.toDouble());
        break;
    case Builtin::FLT:
        ret = (double)args[0]->val.toLongLong();
        break;
    case Builtin::ODD:
        ret = args[0]->val.toLongLong() & 1;
        break;
    case Builtin::ORD:
        ret = args[0]->val;
        break;
    case Builtin::STRLEN:
        ret = ::strlen(args[0]->val.toByteArray().constData());
        break;
    case Builtin::INC:
        if( args.size() == 2 )
            ret = args[0]->val.toLongLong() + args[1]->val.toLongLong();
        else
            ret = args[0]->val.toLongLong() + 1;
        break;
    case Builtin::DEC:
        if( args.size() == 2 )
            ret = args[0]->val.toLongLong() - args[1]->val.toLongLong();
        else
            ret = args[0]->val.toLongLong() - 1;
        break;
    case Builtin::EXCL:
        // TODO
        break;
    case Builtin::INCL:
        // TODO
        break;

    case Builtin::CAP:
        ret = args[0]->val.toChar().toUpper().toLatin1();
        break;
    default:
        return false; // could not be evaluated as a constant
    }

    return true; // when we come here the builtin had const args and was evaluated
}

void Validator::markDecl(Declaration* d)
{
    if( first == 0 )
        return;
    Symbol* s = new Symbol();
    s->kind = Symbol::Decl;
    s->decl = d;
    s->pos = d->pos;
    s->len = d->name.size();
    xref[d].append(s);
    last->next = s;
    last = last->next;
}

Symbol* Validator::markRef(Declaration* d, const RowCol& pos)
{
    if( first == 0 )
        return 0;
    Symbol* s = new Symbol();
    s->kind = Symbol::DeclRef;
    s->decl = d;
    s->pos = pos;
    s->len = d->name.size();
    xref[d].append(s);
    last->next = s;
    last = last->next;
    return s;
}

Declaration*Validator::addHelper(Type* t)
{
    Declaration* helper = new Declaration();
    helper->kind = Declaration::Helper;
    helper->name = "$" + QByteArray::number(++module->id);
    helper->type = t;
    helper->ownstype = true;
    helper->outer = module;
    module->appendMember(helper);
    return helper;
}
