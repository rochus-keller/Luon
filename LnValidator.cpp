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
using namespace Ln;

Validator::Validator(AstModel* mdl, Importer* imp):mdl(mdl),imp(imp)
{
    Q_ASSERT(mdl);
    Q_ASSERT(imp);
}

bool Validator::validate(Declaration* module)
{
    Q_ASSERT(module);
    this->module = module;
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
        visitDecl(cur);
        cur = cur->getNext();
    }
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
    if( d->validating )
    {
        error(d->pos,"circular dependency");
        return;
    }
    d->validating = true;
    switch( d->kind )
    {
    case Declaration::TypeDecl:
    case Declaration::VarDecl:
    case Declaration::LocalDecl:
    case Declaration::ParamDecl:
        if( d->type && d->type->form == Type::NameRef )
            d->type = resolve(d->type);
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
        const QList<Declaration*> params = d->getParams();
        for( int i = 0; i < params.size(); i++ )
            visitDecl(params[i]);
        break;
        }
    default:
        Q_ASSERT(false);
        break;
    }
    d->validated = true;
    d->validating = false;
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

void Validator::visitExpr(Expression* e)
{
    if( e == 0 )
        return;

    switch( e->kind )
    {
    case Expression::Plus:
    case Expression::Minus:
    case Expression::Not: // Unary
        visitExpr(e->lhs);
        unaryOp(e);
        break;
    case Expression::Eq:
    case Expression::Neq:
    case Expression::Lt:
    case Expression::Leq:
    case Expression::Gt:
    case Expression::Geq:
    case Expression::In: // Relation
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
        // NOP
        break;
    case Expression::NameRef:
        resolve(e);
        break;
#if 0
        // TODO
    case Expression::Select:
        visitExpr(e->lhs);
        desigField(e->val.value<Declaration*>(), e->byVal);
        break;
    case Expression::Index:
        visitExpr(e->lhs);
        visitExpr(e->rhs);
        desigIndex(e->byVal);
        break;
    case Expression::Cast:
        visitExpr(e->lhs);
        castPtr(e->type);
        break;
    case Expression::Call:
        {
            ExpList args = e->val.value<ExpList>();
            const DeclList formals = e->lhs->getFormals();
            for(int i = 0; i < args.size(); i++ )
            {
                visitExpr(args[i]);
                if( i < formals.size() )
                    prepareRhs(formals[i]->type);
                else
                    assureTopOnMilStack(); // effects builtin args and variable args
            }
            visitExpr(e->lhs);
            call(args.size());
        }
        break;

    case Expression::Range:
        Constructor
                KeyValue
        // TODO
        break;

#endif
    case Expression::Invalid:
        Q_ASSERT(false);
        break;
    }
}

Type* Validator::resolve(Type* nameRef)
{
    Q_ASSERT(nameRef->form == Type::NameRef);
    Qualident q = nameRef->expr->val.value<Qualident>();
    Declaration* d = find(q, nameRef->expr->pos);
    if( d == 0 )
        return mdl->getType(BasicType::NoType);

    if( !d->validated )
        visitDecl(d);
    if( !d->validated )
        return mdl->getType(BasicType::NoType);

    if( d->kind != Declaration::TypeDecl )
    {
        error(nameRef->expr->pos,"identifier doesn't refer to a type declaration");
        return mdl->getType(BasicType::NoType);
    }

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
        // TODO
        break;
    case Expression::Is:
        // TODO
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

