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

#include "LnAst.h"
#include "LnToken.h"
#include <limits>
#include <QtDebug>
using namespace Ln;

Declaration* AstModel::globalScope = 0;
Type* AstModel::types[BasicType::Max] = {0};

const char* BasicType::name[] = {
    "Undefined",
    "NoType",
    "StrLit",
    "ByteArrayLit",
    "Nil",
    "BOOLEAN",
    "CHAR",
    "BYTE",
    "INTEGER",
    "REAL",
    "SET",
    "STRING",
    "ANYREC"
};

const char* Builtin::name[] = {
    "ABS", "CAP", "BITAND", "BITASR", "BITNOT", "BITOR", "BITS", "BITSHL", "BITSHR",
    "BITXOR", "CAST", "CHR", "CLIP", "DEFAULT", "FLOOR", "FLT", "GETENV", "LEN", "MAX",
    "MIN", "ODD", "ORD", "STRLEN",
    "ASSERT", "COPY", "DEC", "EXCL", "HALT", "INC",
    "INCL", "NEW", "PCALL", "PRINT", "PRINTLN", "RAISE", "SETENV",
    "TRAP", "TRAPIF", "TOSTRING"
};

AstModel::AstModel():helper(0),helperId(0)
{
    if( globalScope == 0 )
    {
        globalScope = new Declaration();
        globalScope->kind = Declaration::Scope;
        openScope(globalScope);
        types[BasicType::Undefined] = newType(BasicType::Undefined,1);
        types[BasicType::NoType] = newType(BasicType::NoType,1);
        types[BasicType::StrLit] = newType(BasicType::StrLit,1);
        types[BasicType::ByteArrayLit] = newType(BasicType::ByteArrayLit,1);
        types[BasicType::Nil] = newType(BasicType::Nil,1);
        types[BasicType::ANYREC] = addType("ANYREC", BasicType::ANYREC,1);

        types[BasicType::BOOLEAN] = addType("BOOLEAN", BasicType::BOOLEAN, 1 );
        types[BasicType::CHAR] = addType("CHAR", BasicType::CHAR, 1 );
        types[BasicType::BYTE] = addType("BYTE", BasicType::BYTE, 1 );
        types[BasicType::INTEGER] = addType("INTEGER", BasicType::INTEGER, 8 );
        types[BasicType::REAL] = addType("REAL", BasicType::REAL, 8 );
        types[BasicType::SET] = addType("SET", BasicType::SET, 4 );
        types[BasicType::STRING] = addType("STRING", BasicType::STRING, 4 );

        addTypeAlias("INT", types[BasicType::INTEGER] );

        for( int i = 0; i < Builtin::Max; i++ )
            addBuiltin(Builtin::name[i], Builtin::Type(i));
    }else
        openScope(globalScope);
}

AstModel::~AstModel()
{
    for( int i = 1; i < scopes.size(); i++ ) // start with 1, 0 is globalScope
        Declaration::deleteAll(scopes[i]);
    scopes.clear();
    Declaration::deleteAll( helper );
}

void AstModel::openScope(Declaration* scope)
{
    if( scope == 0 )
    {
        scope = new Declaration();
        scope->kind = Declaration::Scope;
    }
    scopes.push_back(scope);
}

Declaration* AstModel::closeScope(bool takeMembers)
{
    Declaration* res = 0;

    if( takeMembers )
    {
        res = scopes.back()->link;
        if( res )
            Q_ASSERT(!res->inList);
        scopes.back()->link = 0;
        Declaration::deleteAll(scopes.back());
    }else if( scopes.back()->kind == Declaration::Module )
    {
        // append the helpers to the module so they are guaranteed to have the same life span as the module
        Q_ASSERT(scopes.back()->next == 0);
        scopes.back()->next = helper;
        helper = 0;
        scopes.back()->id = helperId;
        helperId = 0;
    }
    scopes.pop_back();
    return res;
}

Declaration* AstModel::addDecl(const QByteArray& name)
{
    Declaration* scope = scopes.back();

    Declaration* decl = new Declaration();
    decl->name = name;
    if( scope->kind != Declaration::Scope )
        decl->outer = scope;

    if( scope->link == 0 )
        scope->link = decl;
    else
    {
        Declaration* d = scope->link;
        while( d && d->next )
        {
            if( d->name.constData() == name.constData() )
            {
                delete decl;
                return 0; // duplicate
            }
            d = d->next;
        }
        Q_ASSERT( d && d->next == 0 );
        d->next = decl;
        decl->inList = true;
    }
    return decl;
}

Declaration*AstModel::addHelper()
{
    Declaration* decl = new Declaration();
    decl->kind = Declaration::Helper;
    decl->next = helper;
    helper = decl;
    decl->name = "$" + QByteArray::number(++helperId);
    decl->outer = getTopModule();
    return decl;
}

Declaration*AstModel::findDecl(const QByteArray& id, bool recursive) const
{
    for( int i = scopes.size() - 1; i >= 0; i-- )
    {
        Declaration* cur = scopes[i]->link;
        while( cur != 0 )
        {
            if( cur->name.constData() == id.constData() )
                return cur;
            else
                cur = cur->getNext();
        }
        if( !recursive )
            return 0;
    }
    return 0;
}

Declaration*AstModel::findDecl(Declaration* import, const QByteArray& id) const
{
    if( import == 0 )
        return findDecl(id);
    Q_ASSERT(import && import->kind == Declaration::Import);
    Declaration* obj = import->link;
    while( obj != 0 && obj->name.constData() != id.constData() )
        obj = obj->getNext();
    return obj;
}

Declaration*AstModel::getTopScope() const
{
    for( int i = scopes.size() - 1; i >= 0; i-- )
    {
        Declaration* d = scopes[i];
        if( d->kind == Declaration::Module || d->kind == Declaration::Procedure )
            return d;
    }
    return 0;
}

QByteArray AstModel::getTempName()
{
    return Token::getSymbol("$" + QByteArray::number(++helperId));
}

Declaration*AstModel::getTopModule() const
{
    for( int i = 0; i < scopes.size(); i++ )
        if( scopes[i]->kind == Declaration::Module )
            return scopes[i];
    return 0;
}

void AstModel::cleanupGlobals()
{
    if( globalScope )
    {
        Declaration::deleteAll(globalScope);
        globalScope = 0;
        for( int i = 0; i < BasicType::Max; i++ )
        {
            delete types[i];
            types[i] = 0;
        }
    }
}

Type*AstModel::newType(int form, int size)
{
    Type* t = new Type();
    t->form = form;
    return t;
}

Type*AstModel::addType(const QByteArray& name, int form, int size)
{
    Type* t = newType(form, size);
    addTypeAlias(name, t);
    return t;
}

void AstModel::addTypeAlias(const QByteArray& name, Type* t)
{
    Declaration* d = addDecl(Token::getSymbol(name.toUpper()));
    d->validated = true;
    d->kind = Declaration::TypeDecl;
    d->type = t;
    if( t->decl == 0 )
        t->decl = d;
    Declaration* d2 = addDecl(Token::getSymbol(name.toLower()));
    d2->kind = Declaration::TypeDecl;
    d2->type = t;
    d2->validated = true;
}

void AstModel::addBuiltin(const QByteArray& name, Builtin::Type t)
{
    Declaration* d = addDecl(Token::getSymbol(name.toUpper()));
    d->kind = Declaration::Builtin;
    d->type = types[BasicType::NoType];
    d->id = t;
    d = addDecl(Token::getSymbol(name.toLower()));
    d->kind = Declaration::Builtin;
    d->type = types[BasicType::NoType];
    d->id = t;
    d->validated = true;
}

QPair<int, int> Type::countAllocRecordMembers(bool recursive)
{
    QPair<int, int> counts;
    if( form != Record )
        return counts;
    if( recursive && base )
        counts = base->deref()->countAllocRecordMembers(true);

    if( allocated )
        foreach( Declaration* sub, subs )
        {
            if( sub->kind == Declaration::Field )
                counts.first++;
            else if( sub->kind == Declaration::Procedure )
            {
                if( sub->super == 0 )
                    counts.second++;
            }
        }
    else
        foreach( Declaration* sub, subs )
        {
            if( sub->kind == Declaration::Field )
                sub->id = counts.first++;
            else if( sub->kind == Declaration::Procedure )
            {
                if( sub->super == 0 )
                    sub->id = counts.second++;
                else
                    sub->id = sub->super->id;
            }
        }
    allocated = true;

    return counts;
}

bool Type::isSubtype(Type* super, Type* sub)
{
    if( super == 0 || sub == 0 )
        return false;
    while( sub && super != sub )
    {
        sub = sub->base;
        if( sub )
            sub = sub->deref();
    }
    return super == sub;
}

bool Type::isDerefCharArray() const
{
    Type* t = deref();
    if( t && t->form == Array && t->base )
    {
        Type* b = t->base->deref();
        return b && b->form == BasicType::CHAR;
    }
    return false;
}

bool Type::isDerefByteArray() const
{
    Type* t = deref();
    if( t && t->form == Array && t->base )
    {
        Type* b = t->base->deref();
        return b && b->form == BasicType::BYTE;
    }
    return false;
}

Type*Type::deref() const
{
    if( form == NameRef )
    {
        if( base == 0 )
            return const_cast<Type*>(this);
        else
            return base->deref();
    }
    return const_cast<Type*>(this);
}

Declaration*Type::find(const QByteArray& name, bool recursive) const
{
    foreach( Declaration* d, subs)
    {
        if(d->name.constData() == name.constData())
            return d;
    }
    if( recursive && form == Record && base )
        return base->deref()->find(name);
    return 0;
}

QList<Declaration*> Type::fieldList() const
{
    QList<Declaration*> res;
    if( form == Record && base)
        res = base->deref()->fieldList();
    foreach( Declaration* d, subs)
    {
        if( d->kind == Declaration::Field )
            res << d;
        d = d->getNext();
    }
    return res;
}

QList<Declaration*> Type::methodList(bool recursive) const
{
    QList<Declaration*> res;
    if( recursive && form == Record && base)
        res = base->deref()->methodList();
    foreach( Declaration* d, subs)
    {
        if( d->kind == Declaration::Procedure )
            res << d;
        d = d->getNext();
    }
    return res;
}

Type::~Type()
{
    if( form != ConstEnum )
        for( int i = 0; i < subs.size(); i++ )
            Declaration::deleteAll(subs[i]);
    if( expr )
        delete expr;
    // if( base && base->form == NameRef && form != NameRef )
        // A NameRef points to the resolved Type via base which cannot be owned by NameRef per definition
        // On the other hand, each Type which has a base shall delete NameRef
        // NOTE: this doesn't work; base could have been delete before we do the check here
        // delete base;
}

QVariant BasicType::getMax(quint8 form)
{
    switch( form )
    {
    case BOOLEAN:
        return true;
    case CHAR:
        return std::numeric_limits<quint8>::max();
    case SET:
        return 31;
    case BYTE:
        return 255;
    case INTEGER:
        // see https://en.wikipedia.org/wiki/Double-precision_floating-point_format#IEEE_754_double-precision_binary_floating-point_format:_binary64
        // https://pursuit.purescript.org/packages/purescript-int-53/4.0.0/docs/Data.Int53
        return (1 << SignedIntBitWidth)-1; // 9,007,199,254,740,991
    case REAL:
        return std::numeric_limits<double>::max();
    }
    return QVariant();
}

QVariant BasicType::getMin(quint8 form)
{
    switch( form )
    {
    case BOOLEAN:
        return false;
    case CHAR:
        return std::numeric_limits<quint8>::min();
    case BYTE:
        return 0;
    case INTEGER:
        return -(1 << SignedIntBitWidth); // -9,007,199,254,740,991
    case REAL:
        return std::numeric_limits<double>::min();
    }
    return QVariant();
}

Declaration::~Declaration()
{
#if 0
    // use deleteAll instead
    if( next )
        delete next;
#endif
    if( link
            && kind != Declaration::Import  // imports are just referenced, not owned
            )
        Declaration::deleteAll(link);
    if( type && ownstype )
        delete type;
    Statement::deleteAll(body);
    if( expr )
        delete expr;
}

QList<Declaration*> Declaration::getParams(bool skipReceiver) const
{
    Declaration* d = link;
    QList<Declaration*> res;
    while( d && d->kind == Declaration::ParamDecl )
    {
        if( !skipReceiver || d->mode != Declaration::Receiver )
            res << d;
        d = d->next;
    }
    return res;
}

int Declaration::getIndexOf(Declaration* ref) const
{
    int idx = -1;
    Declaration* d = link;
    while( d )
    {
        if( d->kind == ref->kind )
            idx++;
        if( d == ref )
            return idx;
        d = d->next;
    }
    return -1;
}

Declaration*Declaration::getLast() const
{
    Declaration* d = const_cast<Declaration*>(this);
    while( d && d->next )
        d = d->next;
    return d;
}

Declaration*Declaration::find(const QByteArray& name, bool recursive)
{
    Declaration* d = link;
    while( d )
    {
        if( d->name.constData() == name.constData() )
            return d;
        d = d->next;
    }
    if( recursive && outer )
        return outer->find(name);
    return 0;
}

Declaration*Declaration::getModule()
{
    if( kind == Module )
        return this;
    else if( outer )
        return outer->getModule();
    else
        return 0;
}

void Declaration::appendMember(Declaration* decl)
{
    if( link == 0 )
        link = decl;
    else
    {
        Declaration* d = link;
        while( d && d->next )
        {
            d = d->next;
        }
        Q_ASSERT( d && d->next == 0 );
        d->next = decl;
        decl->inList = true;
    }
}

RowCol Declaration::getEndPos() const
{
    Statement* s = body;
    while(s && s->kind != Statement::End )
        s = s->getNext();
    if( s )
        return s->pos;
    else
        return pos;
}

QByteArray Declaration::scopedName(bool withModule, bool withPath) const
{
    QByteArray res;
    const Declaration* d = this;
    while( d && d->kind != Declaration::Module )
    {
        if( !res.isEmpty() )
            res = '$' + res;
        res = d->name + res;
        d = d->outer;
    }
    Q_ASSERT( d && d->kind == Declaration::Module );
    if( withModule )
    {
        if( withPath )
        {
            ModuleData md = d->data.value<ModuleData>();
            res = md.fullName + "." + res;
        }
        res = d->name + "." + res;
    }
    return res;
}

void Declaration::deleteAll(Declaration* d)
{
    if( d )
        Q_ASSERT( !d->inList );
    while( d )
    {
        Declaration* tmp = d->next;
        delete d;
        d = tmp;
    }
}

static inline bool allConst( const Expression* args )
{
    while( args != 0 )
    {
        if( !args->isConst() )
            return false;
        args = args->next;
    }
    return true;
}

bool Expression::isConst() const
{
    switch(kind)
    {
    case DeclRef: {
            Declaration* d = val.value<Declaration*>();
            if( d && ( d->kind == Declaration::VarDecl || d->kind == Declaration::LocalDecl
                       || d->kind == Declaration::ParamDecl ))
                return false;
            else
                return true;
        }
    case ConstVal:
    case Literal:
        return true;
    }

    if( kind == Call )
    {
        Expression* args = rhs;
        if( lhs == 0 )
            return true; // error
        Declaration* d = lhs->val.value<Declaration*>();
        if( d && d->kind == Declaration::Procedure )
        {
            if( d->mode != Declaration::Invar )
                return false;
            else
                return allConst(args);
        }else if(d && d->kind == Declaration::Builtin )
        {
            // TODO: this is no longer required since Validator::callOp removes const calls of builtins
            switch( d->id )
            {
            case Builtin::LEN: {
                    // LEN handles compile-time and dynamic arguments
                    if( getCount(args) != 1 )
                        return true; // error
                    if( args->isConst() )
                        return true;
                    Type* t = args->type ? args->type->deref() : 0;
                    if( t && t->form == Type::Array )
                        return t->len > 0;
                    if( t && t->form == BasicType::STRING )
                        return false;
                    return true;
                }
            case Builtin::MIN:
            case Builtin::MAX:
                return getCount(args) == 1 || allConst(args);
            case Builtin::GETENV:
            case Builtin::DEFAULT:
                return true;
            case Builtin::TOSTRING:
                return false;
            case Builtin::CAST:
                return getCount(args) == 2 && args->next->isConst();
            default:
                return allConst(args);
            }
        }else
            return false;

        return allConst(args);
    }

    if( lhs && !lhs->byName && !lhs->isConst() )
        return false;
    if( rhs && !rhs->isConst() )
        return false;
    return true;
}

bool Expression::isLvalue() const
{
    if( kind == DeclRef )
    {
        Declaration* d = val.value<Declaration*>();
        Q_ASSERT(d);
        return d->kind == Declaration::LocalDecl || d->kind == Declaration::VarDecl || d->kind == Declaration::ParamDecl;
    }
    return kind == Select || kind == Index;
}

void Expression::setByVal()
{
    // go back the desig leaving a ref to type on the stack and mark it to leave a value instead
    Expression* cur = this;
    while( cur && !cur->isLvalue() )
        cur = cur->lhs;
    if( cur )
        cur->byVal = true;
}

bool Expression::isCharLiteral()
{
    if( type == 0 )
        return false;
    if( kind == Literal )
    {
        Type* t = type->deref();
        if( t->form == BasicType::CHAR )
            return true;
        if( t->form == BasicType::StrLit )
        {
            const QByteArray str = val.toByteArray();
            return strlen(str.constData()) == 1;
        }
    }
    return false;
}

qint64 Expression::getCaseValue(bool* ok) const
{
    if(ok)
        *ok = true;
    QVariant v;
    if( kind == DeclRef )
        v = val.value<Declaration*>()->data;
    else
        v = val;
    if( type->isInteger() || type->form == Type::ConstEnum || type->form == BasicType::CHAR )
        return v.toLongLong();
    else if( type->form == BasicType::StrLit )
    {
        const QByteArray str = v.toByteArray();
        // str ends with an explicit zero, thus str.size is 2
        if( strlen(str.constData()) != 1 )
        {
            if(ok)
                *ok = false;
            return 0;
        }else
            return (quint8)str[0];
    }else
    {
        if(ok)
            *ok = false;
        return 0;
    }
}

void Expression::appendRhs(Expression* e)
{
    if( rhs == 0 )
        rhs = e;
    else
        append(rhs,e);
}

int Expression::getCount(const Expression* list)
{
    int count = 0;
    while( list )
    {
        count++;
        list = list->next;
    }
    return count;
}

void Expression::append(Expression* list, Expression* elem)
{
    while( list && list->next )
        list = list->next;
    if( list )
    {
        Q_ASSERT(list->next == 0);
        list->next = elem;
    }
}

QList<Expression*> Expression::getList(Expression* e)
{
    QList<Expression*> res;
    while( e )
    {
        res << e;
        e = e->next;
    }
    return res;
}

Expression*Expression::createFromToken(quint16 tt, const RowCol& rc)
{
    Kind k = Invalid;
    if( tt == Tok_Eq ) {
        k = Eq;
    } else if( tt == Tok_Hash ) {
        k = Neq;
    } else if( tt == Tok_Lt ) {
        k = Lt;
    } else if( tt == Tok_Leq ) {
        k = Leq;
    } else if( tt == Tok_Gt ) {
        k = Gt;
    } else if( tt == Tok_Geq ) {
        k = Geq;
    } else if( tt == Tok_IN ) {
        k = In;
    } else if( tt == Tok_IS ) {
        k = Is;
    } else if( tt == Tok_Plus ) {
        k = Add;
    } else if( tt == Tok_Minus ) {
        k = Sub;
    } else if( tt == Tok_OR ) {
        k = Or;
    } else if( tt == Tok_Star ) {
        k = Mul;
    } else if( tt == Tok_Slash ) {
        k = Fdiv;
    } else if( tt == Tok_DIV ) {
        k = Div;
    } else if( tt == Tok_MOD ) {
        k = Mod;
    } else if( tt == Tok_Amp ) {
        k = And;
    } else if( tt == Tok_AND ) {
        k = And;
    } else if( tt == Tok_integer || tt == Tok_real )
        k = Literal;
    else
        Q_ASSERT(false);
    return new Expression(k,rc);
}

Statement*Statement::getLast() const
{
    Statement* s = const_cast<Statement*>(this);
    while( s && s->next )
        s = s->next;
    return s;
}

void Statement::append(Statement* s)
{
    Q_ASSERT( s && !s->inList );
    Statement* last = getLast();
    last->next = s;
    s->inList = true;
}

Statement::~Statement()
{
    deleteAll(body);
#if 0
    // no recursive delete here, use deleteAll instead
    if( next )
        delete next;
#endif
    if( lhs )
        delete lhs;
    if( rhs )
        delete rhs;
}

void Statement::deleteAll(Statement* s)
{
    if( s )
        Q_ASSERT(!s->inList); // only apply deleteAll to head of list
    while( s )
    {
        Statement* tmp = s->next;
        delete s;
        s = tmp;
    }
}

Ln::Expression::~Expression()
{
    if( lhs )
        delete lhs;
    if( rhs )
        delete rhs;
    if( next )
        delete next;
}

DeclList AstModel::toList(Declaration* d)
{
    if( d == 0 )
        return DeclList();
    Q_ASSERT( !d->inList ); // applies to the head
    DeclList res;
    while( d )
    {
        d->inList = 0;
        res << d;
        Declaration* old = d;
        d = d->next;
        old->next = 0; // the next based list is converted to DeclList, avoid two redundant lists
    }
    return res;
}

void Symbol::deleteAll(Symbol* first)
{
    if( first == 0 )
        return;
    Q_ASSERT( first->kind == Module || first->kind == Invalid );
    Symbol* s = first->next;
    while( s )
    {
        // symbols can build a circle
        if( s == first )
            break;
        Symbol* tmp = s->next;
        delete s;
        s = tmp;
    }
    delete first;
}
