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
#include <QtDebug>
using namespace Ln;

Validator::Validator(AstModel* mdl, Importer* imp):mdl(mdl),imp(imp)
{
    Q_ASSERT(mdl);
    Q_ASSERT(imp);
}

bool Validator::validate(Declaration* module, const Import& import)
{
    Q_ASSERT(module);
    this->module = module;

    Ln::ModuleData md = module->data.value<Ln::ModuleData>();

    md.metaActuals = import.metaActuals;
    md.suffix = imp->moduleSuffix(import.metaActuals);
    if( !md.suffix.isEmpty() )
    {
        md.fullName = Ln::Token::getSymbol(md.fullName + md.suffix);
        module->name = Ln::Token::getSymbol(module->name + md.suffix);
    }
    module->data = QVariant::fromValue(md);

    // TEST import.importedAt.d_row == 470 && import.importer.contains("Havlak"))

    scopeStack.push_back(module);
    for( int i = 0; i < md.metaParams.size(); i++ )
        visitDecl(md.metaParams[i]);
    scopeStack.pop_back();
    if( !import.metaActuals.isEmpty() )
    {
        // trying to instantiate
        if( md.metaParams.size() != md.metaActuals.size() )
        {
            errors << Error("number of formal and actual meta parameters doesn't match",
                            import.importedAt, import.importer);
            return false;
        }
        for( int i = 0; i < md.metaParams.size(); i++ )
        {
            Type* mt = deref(md.metaParams[i]->type);
            Expression* ma = md.metaActuals[i];
#if 0
            // TODO: doesn't work yet because procedure types/consts not yet completely validated
            if( mt && mt->form != Type::Generic && !assigCompat(mt, ma) )
                errors << Error("actual meta type not compatible with meta parameter type",
                                ma->pos, import.importer);
#endif
            if( md.metaParams[i]->kind == Declaration::TypeDecl )
            {
                if( md.metaParams[i]->type && md.metaParams[i]->ownstype )
                    delete md.metaParams[i]->type;
                md.metaParams[i]->type = ma->type;
                md.metaParams[i]->ownstype = false;
            }else
            {
                if( !ma->isConst() )
                    errors << Error("expecting a const expression", ma->pos, import.importer);
                if( md.metaParams[i]->type && md.metaParams[i]->ownstype )
                    delete md.metaParams[i]->type;
                md.metaParams[i]->type = ma->type;
                md.metaParams[i]->data = ma->val;
            }
        }
    }

    try
    {
        visitScope(module);
    }catch(...){ }

    return errors.isEmpty();
}

void Validator::error(const RowCol& pos, const QString& msg)
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
        visitScope(p);
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
    switch( d->kind )
    {
    case Declaration::TypeDecl:
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

    Declaration* mod = imp->loadModule(i);
    if( mod )
    {
        // loadModule returns the module decl; we just need the list of module elements:
        import->link = mod->link;
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
        case Statement::Elsif:
        case Statement::Else:
            break; // ignore, only visited in case of error
        default:
            Q_ASSERT(false);
        }
        s = s->getNext();
    }
}

void Validator::visitExpr(Expression* e, Type* hint)
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
        const QByteArray select = resolve(e);
        if( !select.isEmpty() )
        {
            // the nameref was not a true qualident, but a select
            Expression* nameRef = new Expression();
            *nameRef = *e; // repurpose the new expr as the validated nameref
            e->kind = Expression::Select;
            e->val = select;
            e->lhs = nameRef;
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
        visitExpr(e->lhs); // proc
        visitExpr(e->rhs); // args
        callOp(e);
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
    if( e->next )
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
    Declaration* d = find(q, nameRef->decl->pos);
    if(d == 0)
        return;
    nameRef->validated = true;
    nameRef->base = d->type;
    if( d->kind != Declaration::TypeDecl )
        return error(nameRef->expr->pos,"identifier doesn't refer to a type declaration");
    resolve(d->type);
    if( d->type )
    {
#if 0
        if( d->type->form == Type::Proc )
            visitType(d->type); // RISK: requred so that const meta params are validated when instantiating
            // does not work yet because it triggers circular dependencies
        else
#endif
            resolve(d->type->base);
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
    if( e->kind == Expression::Plus || e->kind == Expression::Minus )
    {
        if( !lhsT->isNumber() )
            return error(e->pos, "unary operator not applicable to this type");
    }else if( e->kind == Expression::Not )
    {
        if( !lhsT->isBoolean() )
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
            e->val = -e->lhs->val.toDouble();
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
    }else if( (lhsT->form == BasicType::CHAR || e->lhs->isCharLiteral()) &&
              (rhsT->form == BasicType::CHAR || e->rhs->isCharLiteral() ) )
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

static qint64 getLabelValue(Expression* e, bool& ok)
{
    ok = true;
    if( e->type->isInteger() || e->type->form == Type::ConstEnum || e->type->form == BasicType::CHAR )
        return e->val.toLongLong();
    else if( e->type->form == BasicType::StrLit )
    {
        const QByteArray str = e->val.toByteArray();
        // str ends with an explicit zero, thus str.size is 2
        if( strlen(str.constData()) != 1 )
        {
            ok = false;
            return 0;
        }else
            return (quint8)str[0];
    }else
    {
        ok = false;
        return 0;
    }
}

static QList<qint64> getNumbers(Expression* e, bool&  ok)
{
    QList<qint64> res;
    if( e->kind == Expression::Range )
    {
        const qint64 a = getLabelValue(e->lhs,ok);
        if( !ok )
            return res;
        const qint64 b = getLabelValue(e->rhs,ok);
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
        res << getLabelValue(e,ok);
    }
    return res;
}

Statement*Validator::caseStat(Statement* s)
{
    visitExpr(s->rhs);
    Expression* caseExp = s->rhs;
    QSet<qint64> labels;
    bool typecase = false;
    bool first = true;
    while(s->getNext() && s->getNext()->kind == Statement::CaseLabel )
    {
        s = s->getNext();
        visitExpr(s->rhs);
        if( s->rhs->kind == Expression::DeclRef && s->rhs->val.value<Declaration*>()->kind == Declaration::TypeDecl )
        {
            // labels for type case
            if( first )
            {
                typecase = true;
                if( deref(caseExp->type)->form != Type::Record )
                    error(caseExp->pos, "type case only supported for record types");
            }
            if( !typecase )
                error(s->pos, "expecting integer, enumeration or char type label");
            else if( s->rhs->next )
                error(s->rhs->next->pos,"only one case label supported per type case");
            else if( !assigCompat(caseExp->type, s->rhs) )
                error(s->rhs->next->pos,"case label type must be an extension of the case expression type");
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
        visitBody(s->body);
    }
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

QByteArray Validator::resolve(Expression* nameRef)
{
    Q_ASSERT( !scopeStack.isEmpty() );

    Qualident q = nameRef->val.value<Qualident>();

    Declaration* d = 0;
    QByteArray select;
    if( !q.first.isEmpty() )
    {
        // check whether this is a local name or an import
        d = scopeStack.back()->find(q.first);
        if( d == 0 )
        {
            error(nameRef->pos,QString("declaration '%1' not found").arg(q.first.constData()));
            return QByteArray();
        }else if( d->kind == Declaration::Import )
            d = find(q,nameRef->pos, d);
        else
        {
            // this is a local select, not an import
            select = q.second;
            q.second = q.first;
            q.first.clear();
            nameRef->val = QVariant::fromValue(q);
        }
    }else
        d = find(q,nameRef->pos);
    if( d == 0 )
        return QByteArray();

    resolve(d->type);
#if 0
    visitDecl(d); // RISK: we need that so that procedures are resolved when passing as meta actuals
    // does not work yet because it triggers circular dependencies
#endif

    // repurpose the expression
    if( d->kind == Declaration::ConstDecl && d->mode != Declaration::Enum )
    {
        nameRef->val = d->data;
        nameRef->type = d->type;
        toConstVal(nameRef);
    }else
    {
        // Enum ConstDecls are handled here because they are used as symbols in the first place
        toConstVal(nameRef);
        nameRef->kind = Expression::DeclRef;
        nameRef->val = QVariant::fromValue(d);
        nameRef->type = d->type;
    }
    return select;
}

Declaration* Validator::find(const Qualident& q, const RowCol& pos, Declaration* import)
{
    if( scopeStack.isEmpty() )
        return 0;

    Declaration* d = 0;
    if( !q.first.isEmpty() )
    {
        if( import == 0 )
            import = scopeStack.back()->find(q.first);
        if( import == 0 || import->kind != Declaration::Import )
        {
            error(pos,"identifier doesn't refer to an imported module");
            return 0;
        }
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
            d = member;
        }
    }else
    {
        d = scopeStack.back()->find(q.second);
        if( d == 0 )
            d = mdl->findDecl(q.second);
        if( d == 0 )
            error(pos,QString("declaration '%1' not found").arg(q.second.constData()));
    }
    return d;
}

void Validator::selectOp(Expression* e)
{
    if( e->lhs == 0 || e->lhs->type == 0 )
        return;
    Type* lhsT = deref(e->lhs->type);
    if( lhsT->form == Type::Record )
    {
        Declaration* field = lhsT->find(e->val.toByteArray());
        if( field == 0 )
            error(e->pos,QString("the record doesn't have a field named '%1'"). arg(e->val.toString()) );
        else
        {
            e->val = QVariant::fromValue(field); // Field or bound proc
            e->type = field->type;
        }
    }else
        error(e->pos,"cannot select a field in given type");
}

void Validator::callOp(Expression* e)
{
    if( e->lhs == 0 || e->lhs->type == 0 ) // e->rhs is null in case there are no args
        return;

    Declaration* proc = 0;
    if( e->lhs->kind == Expression::DeclRef || e->lhs->kind == Expression::Select )
    {
        proc = e->lhs->val.value<Declaration*>();
        if( proc && proc->kind == Declaration::Field )
            proc = 0; // this must be a proc type field
    }else if( e->lhs->kind == Expression::ConstVal && e->lhs->val.canConvert<Declaration*>() )
        proc = e->lhs->val.value<Declaration*>(); // happens if a procedure is passed in via meta actual


    const bool isTypeCast = (proc == 0 || proc->kind != Declaration::Builtin) &&
            e->rhs &&
            e->rhs->kind == Expression::DeclRef &&
            e->rhs->val.value<Declaration*>()->kind == Declaration::TypeDecl;

    Type* lhsT = deref(e->lhs->type);
    if( isTypeCast )
    {
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
                return error(e->lhs->pos,"this expression cannot be called");
            e->type = proc->type;
        }else if( lhsT->form != Type::Proc )
            return error(e->lhs->pos,"this expression cannot be called");
        else
            e->type = lhsT->base;

        QList<Expression*> actuals = Expression::getList(e->rhs);

        if( proc && proc->kind == Declaration::Builtin )
        {
            if( proc->id == Builtin::ASSERT )
            {
                Expression* ee = new Expression(Expression::Literal,e->pos);
                ee->type = mdl->getType(BasicType::INTEGER);
                ee->val = e->lhs->pos.d_row;
                Expression::appendArg(e->rhs,ee);
                ee = new Expression(Expression::Literal,e->pos);
                ee->type = mdl->getType(BasicType::StrLit);
                ModuleData md = module->data.value<ModuleData>();
                ee->val = md.fullName;
                Expression::appendArg(e->rhs,ee);
            }
            checkBuiltinArgs(proc->id, actuals, &e->type, e->pos);
        }else
        {
            const DeclList formals = e->lhs->getFormals();

            if( actuals.size() < formals.size() )
                error(e->pos,"not enough actual arguments");

            for( int i = 0; i < formals.size() && i < actuals.size(); i++ )
            {
                if( !paramCompat(formals[i],actuals[i]) )
                {
                    //paramCompat(formals[i],actuals[i]); // TEST
                    error(actuals[i]->pos, "actual argument not compatible with formal parameter");
                }
            }
        }

        // TODO: call if const builtin or invar
    }
}

void Validator::callOp(Statement* s)
{
    Q_ASSERT( s->lhs );
    if( s->lhs->kind != Expression::Call )
    {
        Expression* call = new Expression(Expression::Call, s->pos);
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
    if( !Type::isSubtype(lhsT, rhsT) )
        return error(e->rhs->pos,"rhs type must be an extension of lhs type"); // TODO: ANYREC
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

void Validator::constructor(Expression* e, Type* hint)
{
    // e->type is preset if NamedType is present
    // otherwise we use hint, or SET
    if( e->type == 0 )
    {
        if( hint )
            e->type = deref(hint);
        else
            e->type = mdl->getType(BasicType::SET);
    }
    Expression* c = e->rhs;
    QList<Declaration*> fields;
    if( e->type->form == Type::Record)
        fields = e->type->fieldList();
    int index = 0;
    bool allConst = true;
    while( c )
    {
        // go through all components
        if( e->type->form == Type::Record )
        {
            if( c->kind == Expression::KeyValue )
            {
                if( !c->byName )
                    return error(c->pos,"index components not supported in record constructors");
                Q_ASSERT( c->lhs->kind == Expression::NameRef );
                Qualident q = c->lhs->val.value<Qualident>();
                index = find_(fields, q.second);
                if( index < 0 )
                    return error(c->pos,"field not known in record or its base records");
                visitExpr(c->rhs, fields[index]->type);
                c->lhs->val = QVariant::fromValue(fields[index]);
                c->type = c->rhs->type;
                if( c->rhs->kind == Expression::Range )
                    return error(c->pos,"cannot use a range here");
            }else
            {
                if( c->kind == Expression::Range )
                    return error(c->pos,"cannot use a range here");
                if( index >=  fields.size() )
                    return error(c->pos,"component position out of record");
                visitExpr(c, fields[index]->type);
            }
            if( !assigCompat(fields[index]->type,c->type) )
                return error(c->pos,"component type incompatible with field type");
        }else if( e->type->form == Type::Array )
        {
            if( c->kind == Expression::KeyValue )
            {
                if( c->byName )
                    return error(c->pos,"label components not supported in record constructors");
                visitExpr(c->lhs);
                visitExpr(c->rhs, e->type->base);
                c->type = c->rhs->type;
                if( c->rhs->kind == Expression::Range )
                    return error(c->pos,"cannot use a range here");
            }else
            {
                if( c->kind == Expression::Range )
                    return error(c->pos,"cannot use a range here");
                if( e->type->len && index >= e->type->len )
                    return error(c->pos,"component position out of array");
                visitExpr(c, e->type->base);
            }
            if( !assigCompat(e->type->base,c->type) )
                return error(c->pos,"component type incompatible with element type");
        }else if( e->type->form == Type::HashMap )
        {
            // TODO: a : b could be the same as ["a"] : b, but then we should also support h.a for h["a"]
            if( c->kind == Expression::KeyValue )
            {
                if( c->byName )
                    return error(c->pos,"label components not supported in record constructors");
                visitExpr(c->lhs);
                if( e->type->expr && !assigCompat(e->type->expr->type, c->lhs->type) )
                    return error(c->pos,"index type of component not compatible with key type");
                visitExpr(c->rhs, e->type->base);
                c->type = c->rhs->type;
                if( c->rhs->kind == Expression::Range )
                    return error(c->pos,"cannot use a range here");
            }else
                return error(c->pos,"only key-value-components supported for HASHMAP constructors");
        }else if( e->type->form == BasicType::SET )
        {
            if( c->kind == Expression::KeyValue || c->kind == Expression::Constructor )
                return error(c->pos,"component type not supported for SET constructors");
            visitExpr(c);
            if( c->kind == Expression::Range && !sameType(c->lhs->type, c->rhs->type) )
                error(e->pos,"types in range must be the same");
            if( c->type && !deref(c->type)->isInteger() )
                return error(c->pos,"expecting integer compontents for SET constructors");
        }else
        {
            return error(e->pos,"constructors cannot be used to create the given type");
            // stop immediately
        }
        if( !c->isConst() )
            allConst = false;
        index++;
        c = c->next;
    }
    if( e->type->form == BasicType::SET && allConst )
    {
        Expression* c = e->rhs;
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
                    for(int i = a; a <= b; i++ )
                        bits.set(i);
                else
                    for(int i = b; b <= a; i++ )
                        bits.set(i);
            }else
            {
                const quint64 n = c->val.toLongLong();
                if( n < 0 || n > 31 )
                    error(c->pos,"set component out of range");
                else
                    bits.set(n);
            }
        }
        toConstVal(e);
        e->val = (qint64)bits.to_ulong();
    }
}

Type*Validator::deref(Type* t) const
{
    if( t == 0 )
        return 0;
    if( t->form == Type::NameRef )
    {
        if( !t->validated )
        {
            Qualident q = t->decl->data.value<Qualident>();
            qWarning() << "using unresolved NameRef:" << q.first.constData() << q.second.constData();
        }
        if( t->base == 0 )
            return t;
        return deref(t->base);
    }else
        return t;
}

bool Validator::assigCompat(Type* lhs, Type* rhs) const
{
    lhs = deref(lhs);
    rhs = deref(rhs);
    if( lhs == 0 || rhs == 0 )
        return false;

    if( sameType(lhs,rhs) )
        return true;

    if( lhs->form == Type::Array && rhs->form == Type::Array && (lhs->len == 0 || lhs->len == rhs->len) &&
            equalTypes(lhs->base, rhs->base) )
        return true;

    if( ( lhs->form == Type::Record || lhs->form == Type::Array ||
          lhs->form == Type::HashMap || lhs->form == Type::Proc ) && rhs->form == BasicType::Nil )
        return true;

    if( lhs->form == Type::Record && rhs->form == Type::Record &&
            Type::isSubtype(lhs,rhs) )
        return true;

    if( lhs->form == Type::Proc && rhs->form == Type::Proc )
        return matchFormals(lhs->subs, rhs->subs) && matchResultType(lhs->base,rhs->base);

    return false;
}

bool Validator::assigCompat(Type* lhs, Declaration* rhs) const
{
    if( rhs->kind == Declaration::TypeDecl )
        return false;

    if( lhs == 0 )
        return false;

    lhs = deref(lhs);

    // Tv is a procedure type and e is the name of a procedure whose formal parameters match those of Tv.
    if( lhs->form == Type::Proc && rhs->kind == Declaration::Procedure )
        return matchFormals(lhs->subs, rhs->getParams()) && matchResultType(lhs->base,rhs->type);

    // Tv is an enumeration type and e is a valid element of the enumeration;
    if( lhs->form == Type::ConstEnum )
        return lhs->subs.contains(rhs);

    return assigCompat(lhs, rhs->type);
}

bool Validator::assigCompat(Type* lhs, const Expression* rhs) const
{
    lhs = deref(lhs);
    if( lhs == 0 || rhs == 0 || rhs->type == 0 )
        return false;

    Type* rhsT = deref(rhs->type);
    if( rhs->kind == Expression::DeclRef )
        return assigCompat(lhs, rhs->val.value<Declaration*>() );

    // A string of length 1 can be used wherever a character constant is allowed and vice versa.
    if( lhs->form == BasicType::CHAR && rhsT->form == BasicType::StrLit )
        return strlen(rhs->val.toByteArray().constData()) == 1;

    return assigCompat(lhs, rhs->type);
}

bool Validator::paramCompat(Declaration* lhs, const Expression* rhs) const
{
    Q_ASSERT(lhs->kind == Declaration::ParamDecl);

    Type* lhsT = deref(lhs->type);
    Type* rhsT = deref(rhs->type);
    if( lhsT == 0 || rhsT == 0 )
        return false;
    // a string literal (which is a STRING actually) is compatible with an const array of char formal param
    if( lhs->visi == Declaration::ReadOnly && lhsT->isDerefCharArray() && lhsT->len == 0 &&
            (rhsT->form == BasicType::StrLit || rhsT->form == BasicType::STRING ) )
        return true;

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
    lhs = deref(lhs);
    rhs = deref(rhs);
    if( lhs == 0 || rhs == 0 )
        return false;
    return sameType(lhs,rhs) || (lhs->form == BasicType::NoType && rhs->form == BasicType::NoType);
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

    if( lhs == 0 || rhs == 0 )
        return false;

    // Ta and Tb are open array types with equal element types, or
    // Ta and Tb are non-open array types with same length and equal element types, or
    if( lhs->form == Type::Array && rhs->form == Type::Array && lhs->len == rhs->len &&
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

static inline bool expectingNArgs(const QList<Expression*>& args,int n)
{
    if( args.size() != n )
        throw QString("expecting %1 arguments").arg(n);
    for( int i = 0; i < args.size(); i++ )
        if( args[i]->type == 0 )
            return false;
    return true;
}

static inline bool expectingNMArgs(const QList<Expression*>& args,int n, int m)
{
    if( args.size() < n || args.size() > m)
        throw QString("expecting %1 to %2 arguments").arg(n).arg(m);
    for( int i = 0; i < args.size(); i++ )
        if( args[i]->type == 0 )
            return false;
    return true;
}

static void checkBitArith(const QList<Expression*>& args, Type** ret)
{
    if( !expectingNArgs(args,2) )
        return;
    if( !args[0]->type->deref()->isInteger() )
        throw QString("expecing integer first argument");
    if( !args[1]->type->deref()->isInteger() )
        throw QString("expecing integer second argument");
    *ret = args[0]->type;
}

bool Validator::checkBuiltinArgs(quint8 builtin, const QList<Expression*>& args, Type** ret, const RowCol& pos)
{
    Q_ASSERT(ret);

    *ret = mdl->getType(BasicType::NoType);

    // TODO:
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
    case Builtin::CAP:
        expectingNArgs(args,1);
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
        expectingNArgs(args,2);
        break;
    case Builtin::CHR:
        expectingNArgs(args,1);
        break;
    case Builtin::DEFAULT:
        if(!expectingNArgs(args,1))
            break;
        *ret = args[0]->type;
        break;    
    case Builtin::EQUALS:
        if( !expectingNArgs(args,2) )
            break;
        *ret = mdl->getType(BasicType::BOOLEAN);
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
    case Builtin::GETENV:
        expectingNArgs(args,2);
        break;
    case Builtin::LEN:
        expectingNArgs(args,1);
        *ret = mdl->getType(BasicType::INTEGER);
        break;
    case Builtin::MAX:
        if(!expectingNMArgs(args,1,2))
            break;
        *ret = args[0]->type;
        break;
    case Builtin::MIN:
        if(!expectingNMArgs(args,1,2))
            break;
        *ret = args[0]->type;
        break;
    case Builtin::ODD:
        expectingNArgs(args,1);
        break;
    case Builtin::ORD:
        expectingNArgs(args,1);
        break;
    case Builtin::SIZE:
        expectingNArgs(args,1);
        break;
    case Builtin::STRLEN:
        expectingNArgs(args,1);
        break;
    case Builtin::VARARG:
        expectingNMArgs(args,2,3);
        break;
    case Builtin::VARARGS:
        expectingNArgs(args,0);
        break;

    // procedures:
    case Builtin::ASSERT:
        expectingNArgs(args,1);
        break;
    case Builtin::COPY:
        expectingNArgs(args,2);
        break;
    case Builtin::DEC:
        expectingNMArgs(args,1,2);
        break;
    case Builtin::EXCL:
        expectingNArgs(args,2);
        break;
    case Builtin::HALT:
        expectingNArgs(args,1);
        break;
    case Builtin::INC:
        expectingNMArgs(args,1,2);
        break;
    case Builtin::INCL:
        expectingNArgs(args,2);
        break;
    case Builtin::NEW:
        expectingNMArgs(args,1,2);
        break;
    case Builtin::PCALL:
        break;
    case Builtin::PRINT:
        expectingNArgs(args,1);
        break;
    case Builtin::PRINTLN:
        expectingNArgs(args,1);
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
