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
            if( md.metaParams[i]->kind == Declaration::TypeDecl )
            {
                md.metaParams[i]->type = md.metaActuals[i]->type;
                md.metaParams[i]->ownstype = false;
            }else
            {
                // TODO
            }
        }
    }

    visitScope(module);
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
        visitBody(p);
    cur = scope->link;
    while( cur )
    {
        if( cur->kind == Declaration::Procedure )
            visitBody(cur);
        cur = cur->getNext();
    }
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
        if( d->type && d->type->form == Type::NameRef )
            d->type = resolve(d->type);
        else
            visitType(d->type);
        break;
    case Declaration::ConstDecl:
        visitExpr(d->expr);
        if( d->expr )
            d->type = d->expr->type;
        else
            d->type = mdl->getType(BasicType::NoType);
        break;
    case Declaration::Import:
        visitImport(d);
        break;
    case Declaration::Procedure: {
        // only header is evaluated here
        if( d->type && d->type->form == Type::NameRef )
            d->type = resolve(d->type);
        else
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
    Import i = import->data.value<Import>();
    foreach( Expression* e, i.metaActuals )
        visitExpr(e);

    Declaration* mod = imp->loadModule(i);
    if( mod )
    {
        // loadModule returns the module decl; we just need the list of module elements:
        import->link = mod->link;
    }
    import->validated = true;
}

void Validator::visitBody(Declaration* body)
{
    if( body->validated )
        return;
    // TODO
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
        // NOP
        break;
    case Expression::NameRef:
        resolve(e);
        break;
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
    if( type->base && type->base->form == Type::NameRef )
        type->base = resolve(type->base);
    else
        visitType(type->base);
    if( type->expr )
    {
        visitExpr(type->expr );
        if( type->form == Type::Array )
        {
            if( !type->expr->isConst() || !type->expr->type->isInteger() )
                error(type->expr->pos,"expecting constant integer expression");
            else
            {
                const qint64 tmp = type->expr->val.toLongLong();
                if( tmp < 0 || tmp > std::numeric_limits<quint32>::max() )
                    error(type->expr->pos,"invalid array length");
                else
                    type->len = tmp;
            }
        }else if( type->form == Type::HashMap && type->expr->kind == Expression::DeclRef )
        {
            if( type->expr->val.value<Declaration*>()->kind != Declaration::TypeDecl )
                error(type->expr->pos,"expecting a type name");
        }
    }
    foreach( Declaration* d, type->subs )
    {
        if( d->kind == Declaration::Procedure )
            boundProcs << d; // do body later
        visitDecl(d);
    }
}

Type* Validator::resolve(Type* nameRef)
{
    Q_ASSERT(nameRef->form == Type::NameRef);
    Qualident q = nameRef->expr->val.value<Qualident>();
    Declaration* d = find(q, nameRef->expr->pos);
    if( d == 0 )
        return mdl->getType(BasicType::NoType);

    if( d->kind != Declaration::TypeDecl )
    {
        error(nameRef->expr->pos,"identifier doesn't refer to a type declaration");
        return mdl->getType(BasicType::NoType);
    }

    if( !d->validating && !d->validated )
        visitDecl(d);

    return d->type;
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
    if( e->kind == Expression::Plus || e->kind == Expression::Minus )
    {
        if( !e->lhs->type->isNumber() )
            return error(e->pos, "unary operator not applicable to this type");
    }else if( e->kind == Expression::Not )
    {
        if( !e->lhs->type->isBoolean() )
            return error(e->pos, "unary '~' or 'NOT' not applicable to this type");
    }
    if( e->lhs == 0 )
        return; // already reported
    e->type = e->lhs->type;

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
    e->type = e->lhs->type;
    if( e->lhs->type->isNumber() && e->rhs->type->isNumber() )
    {
        if( e->lhs->type->isInteger() && e->rhs->type->isInteger() )
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
        else if( e->lhs->type->isReal() && e->rhs->type->isReal() )
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
    }else if( e->lhs->type->isSet() && e->rhs->type->isSet() )
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
    }else if(e->lhs->type->isBoolean() && e->rhs->type->isBoolean())
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
    }else if((e->lhs->type->form == BasicType::StrLit || e->lhs->type->form == BasicType::CHAR) &&
             (e->rhs->type->form == BasicType::StrLit || e->rhs->type->form == BasicType::CHAR))
    {
        if( e->kind != Expression::Add )
            error(e->pos,"only the '+' operator can be applied to string and char literals");
        else if( !e->isConst() )
            error(e->pos,"operation is only available for string and char literals");
        else
        {
            QByteArray lhs;
            QByteArray rhs;
            if( e->lhs->type->form == BasicType::CHAR)
                lhs = QByteArray(1,(char)e->lhs->val.toUInt());
            else
                lhs = e->lhs->val.toByteArray();
            if( e->rhs->type->form == BasicType::CHAR)
                rhs = QByteArray(1,(char)e->rhs->val.toUInt());
            else
                rhs = e->rhs->val.toByteArray();
            e->val = lhs + rhs;
            toConstVal(e);
            e->type = mdl->getType(BasicType::StrLit);
        }
    }else
        error(e->pos,"operands not compatible with operator");
}

void Validator::relOp(Expression* e)
{
    e->type = mdl->getType(BasicType::BOOLEAN);
    if( e->lhs->type->isNumber() && e->rhs->type->isNumber() )
    {
        if( e->lhs->type->isInteger() && e->rhs->type->isInteger() ||
                e->lhs->type->form == Type::ConstEnum  && e->rhs->type->form == Type::ConstEnum )
        {
            if( e->lhs->type->form == Type::ConstEnum  && e->rhs->type->form == Type::ConstEnum &&
                    e->lhs->type != e->rhs->type )
                error(e->pos, "cannot compare the elements of different enumeration types");

            switch(e->kind)
            {
            case Expression::Geq:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toLongLong() >= e->rhs->val.toLongLong();
                    toConstVal(e);
                }
                break;
            case Expression::Gt:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toLongLong() > e->rhs->val.toLongLong();
                    toConstVal(e);
                }
                break;
            case Expression::Eq:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toLongLong() == e->rhs->val.toLongLong();
                    toConstVal(e);
                }
                break;
            case Expression::Leq:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toLongLong() <= e->rhs->val.toLongLong();
                    toConstVal(e);
                }
                break;
            case Expression::Neq:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toLongLong() != e->rhs->val.toLongLong();
                    toConstVal(e);
                }
                break;
            case Expression::Lt:
                if( e->isConst() )
                {
                    e->val = e->lhs->val.toLongLong() < e->rhs->val.toLongLong();
                    toConstVal(e);
                }
                break;
            default:
                error(e->pos,"operator not supported for integer operands");
                break;
            }
        }else if(e->lhs->type->isReal() && e->rhs->type->isReal() )
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
    }else if( e->lhs->type->isReference() && e->rhs->type->form == BasicType::Nil ||
              e->lhs->type->form == BasicType::Nil && e->rhs->type->isReference() ||
              e->lhs->type->form == BasicType::Nil && e->rhs->type->form == BasicType::Nil ||
              e->lhs->type->isReference() && e->rhs->type->isReference() ||
              e->lhs->type->isSet() && e->rhs->type->isSet() ||
              e->lhs->type->isBoolean() && e->rhs->type->isBoolean() )
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
    }else
        error(e->pos, "operands not compatible with operator");
}

void Validator::resolve(Expression* nameRef)
{
    // NOTE: NameRef is used as designator or by named type; the former is handled here, the latter in resolve(Type*)
    Qualident q = nameRef->val.value<Qualident>();
    Declaration* d = find(q,nameRef->pos);
    if( d == 0 )
        return;
    if( !d->validated )
        visitDecl(d);
    if( !d->validated )
        return;
    // repurpose the expression
    if( d->kind == Declaration::ConstDecl )
    {
        nameRef->val = d->data;
        nameRef->type = d->type;
        toConstVal(nameRef);
    }else
    {
        toConstVal(nameRef);
        nameRef->kind = Expression::DeclRef;
        nameRef->val = QVariant::fromValue(d);
        nameRef->type = d->type;
    }
}

Declaration* Validator::find(const Qualident& q, const RowCol& pos)
{
    if( scopeStack.isEmpty() )
        return 0;

    Declaration* d = 0;
    if( !q.first.isEmpty() )
    {
        Declaration* import = scopeStack.back()->find(q.first);
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
    if( e->lhs->type->form == Type::Record )
    {
        Declaration* field = e->type->find(e->val.toByteArray());
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
    if( e->lhs == 0 || e->lhs->type == 0 || e->rhs == 0 ||e->rhs->type == 0 )
        return;

    Declaration* proc = 0;
    if( e->lhs->kind == Expression::DeclRef )
        proc = e->lhs->val.value<Declaration*>();

    const bool isTypeCast = (proc == 0 || proc->kind != Declaration::Builtin) &&
            e->rhs->kind == Expression::DeclRef &&
            e->rhs->val.value<Declaration*>()->kind == Declaration::TypeDecl;

    if( isTypeCast )
    {
        e->kind = Expression::Cast;
        e->type = e->lhs->type;
        if( e->rhs->next )
            return error(e->rhs->next->pos,"type guard requires a single argument");
        if( e->lhs->type->form != Type::Record )
            return error(e->rhs->pos,"a type guard is not supported for this type");
    }else
    {
        if( proc )
        {
            if( proc->kind != Declaration::Procedure && proc->kind != Declaration::Builtin )
                return error(e->lhs->pos,"this expression cannot be called");
            e->type = e->lhs->type;
        }else if( e->lhs->type->form != Type::Proc )
            return error(e->lhs->pos,"this expression cannot be called");
        else
            e->type = e->lhs->type->base;

        const DeclList formals = e->lhs->getFormals();
        QList<Expression*> actuals = Expression::getList(e->rhs);

        if( proc && proc->kind != Declaration::Builtin && actuals.size() < formals.size() )
            error(e->pos,"not enough actual arguments");

        for( int i = 0; i < formals.size() && i < actuals.size(); i++ )
        {
            if( !paramCompat(formals[i],actuals[i]) )
                error(actuals[i]->pos, "actual argument not compatible with formal parameter");
        }

        if( proc && proc->kind != Declaration::Builtin && proc->id == Builtin::ASSERT )
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

        // TODO: call if const builtin or invar
    }
}

void Validator::indexOp(Expression* e)
{
    if( e->lhs == 0 || e->lhs->type == 0 || e->rhs == 0 ||e->rhs->type == 0 )
        return;

    e->type = e->lhs->type->base;
    if( e->lhs->type->form == Type::Array )
    {
        if( !e->rhs->type->isInteger() )
            error(e->rhs->pos,"expecting an array index of integer type");
    }else if(e->lhs->type->form == Type::HashMap)
    {
        if( !assigCompat(e->lhs->type->expr->type, e->rhs->type ) )
            error(e->rhs->pos,"expression not compatible with key type");
    }else
        error(e->pos,"cannot index an element in given type");
}

void Validator::inOp(Expression* e)
{
    if( e->lhs == 0 || e->lhs->type == 0 || e->rhs == 0 ||e->rhs->type == 0 )
        return;
    e->type = mdl->getType(BasicType::BOOLEAN);
    if( !e->lhs->type->isInteger() )
        return error(e->lhs->pos,"expecting integer type");
    if( !e->rhs->type->isSet() )
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
    e->type = mdl->getType(BasicType::BOOLEAN);
    if( e->lhs->type->form != Type::Record )
        return error(e->lhs->pos,"expecting record type");
    if( e->rhs->type->form != Type::Record )
        return error(e->rhs->pos,"expecting record type");
    if( e->rhs->kind != Expression::DeclRef || e->rhs->val.value<Declaration*>()->kind != Declaration::TypeDecl )
        return error(e->rhs->pos,"expecting a type declaration");
    if( !Type::isSubtype(e->lhs->type, e->rhs->type) )
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
            e->type = hint;
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
            if( c->type && !c->type->isInteger() )
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

bool Validator::assigCompat(Type* lhs, Type* rhs) const
{
    if( lhs == 0 || rhs == 0 )
        return false;

    if( sameType(lhs,rhs) )
        return true;


    // Te and Tv are non-open array types with the same length and have equal base types;
    if( lhs->form == Type::Array && rhs->form == Type::Array && lhs->len != 0 && lhs->len == rhs->len &&
            equalTypes(lhs->base, rhs->base) )
        return true;

    // Tv is a pointer or a procedure type and e is NIL;
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

    // Tv is a procedure type and e is the name of a procedure whose formal parameters match those of Tv.
    if( lhs->form == Type::Proc && rhs->mode == Declaration::Procedure )
        return matchFormals(lhs->subs, rhs->getParams()) && matchResultType(lhs->base,rhs->type);

    // Tv is an enumeration type and e is a valid element of the enumeration;
    if( lhs->form == Type::ConstEnum )
        return lhs->subs.contains(rhs);

    if( lhs->form == Type::Array && lhs->base->form == BasicType::CHAR && lhs->len > 0 &&
            rhs->mode == Declaration::ConstDecl && rhs->type->form == BasicType::StrLit )
        return strlen(rhs->data.toByteArray().constData()) < lhs->len;

    return assigCompat(lhs, rhs->type);
}

bool Validator::assigCompat(Type* lhs, const Expression* rhs) const
{
    if( lhs == 0 || rhs == 0 )
        return false;

    if( rhs->kind == Expression::DeclRef )
        return assigCompat(lhs, rhs->val.value<Declaration*>() );

    if( lhs->form == Type::Array && lhs->base->form == BasicType::CHAR && lhs->len > 0 &&
            ( rhs->kind == Expression::Literal ||rhs->kind == Expression::ConstVal ) &&
            rhs->type->form == BasicType::StrLit)
        return strlen(rhs->val.toByteArray().constData()) < lhs->len;

    // A string of length 1 can be used wherever a character constant is allowed and vice versa.
    if( lhs->form == BasicType::CHAR && rhs->type->form == BasicType::StrLit )
        return strlen(rhs->val.toByteArray().constData()) == 1;

    return assigCompat(lhs, rhs->type);
}

bool Validator::paramCompat(Declaration* lhs, const Expression* rhs) const
{
    Q_ASSERT(lhs->mode == Declaration::ParamDecl);

#if 0
    // TODO
    // Tf is a pointer to an open array of CHAR, f is CONST, and a is string literal
    if( lhs->visi == Declaration::ReadOnly &&
            lhs->type->base->form == Type::Array && lhs->type->base->base->form == BasicType::CHAR &&
            lhs->type->base->len == 0 && rhs->type->form == BasicType::StrLit )
        return true;
#endif

    // Tf and Ta are equal types, or Ta is assignment compatible with Tf
    return equalTypes(lhs->type,rhs->type) || assigCompat(lhs->type,rhs);
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
    if( lhs == 0 || rhs == 0 )
        return false;
    return sameType(lhs,rhs) || (lhs->form == BasicType::NoType && rhs->form == BasicType::NoType);
}

bool Validator::sameType(Type* lhs, Type* rhs) const
{
    return lhs == rhs;
}

bool Validator::equalTypes(Type* lhs, Type* rhs) const
{
    // Ta and Tb are the same type,
    if(sameType(lhs,rhs))
        return true;

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
