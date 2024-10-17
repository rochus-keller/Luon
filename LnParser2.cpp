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

#include "LnParser2.h"
using namespace Ln;

static const quint64 maxUlong = 0x3fffffffffffff; // 53 bits

static inline bool FIRST_Luon(int tt) {
	return tt == Tok_MODULE;
}

static inline bool FIRST_number(int tt) {
	return tt == Tok_integer || tt == Tok_real;
}

static inline bool FIRST_qualident(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_identdef(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_ConstDeclaration(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_ConstExpression(int tt) {
	switch(tt){
	case Tok_hexstring:
	case Tok_NOT:
	case Tok_Tilde:
	case Tok_NIL:
	case Tok_Lbrace:
	case Tok_ident:
	case Tok_TRUE:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_Plus:
	case Tok_string:
	case Tok_real:
	case Tok_FALSE:
	case Tok_Lpar:
	case Tok_Minus:
		return true;
	default: return false;
	}
}

static inline bool FIRST_TypeDeclaration(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_type(int tt) {
	switch(tt){
	case Tok_PROC:
	case Tok_RECORD:
	case Tok_HASHMAP:
	case Tok_ident:
	case Tok_ARRAY:
	case Tok_PROCEDURE:
	case Tok_Lbrack:
	case Tok_Lpar:
		return true;
	default: return false;
	}
}

static inline bool FIRST_NamedType(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_ArrayType(int tt) {
	return tt == Tok_ARRAY || tt == Tok_Lbrack;
}

static inline bool FIRST_length(int tt) {
	switch(tt){
	case Tok_hexstring:
	case Tok_NOT:
	case Tok_Tilde:
	case Tok_NIL:
	case Tok_Lbrace:
	case Tok_ident:
	case Tok_TRUE:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_Plus:
	case Tok_string:
	case Tok_real:
	case Tok_FALSE:
	case Tok_Lpar:
	case Tok_Minus:
		return true;
	default: return false;
	}
}

static inline bool FIRST_DictType(int tt) {
	return tt == Tok_HASHMAP;
}

static inline bool FIRST_RecordType(int tt) {
	return tt == Tok_RECORD;
}

static inline bool FIRST_BaseType(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_FieldList(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_IdentList(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_enumeration(int tt) {
	return tt == Tok_Lpar;
}

static inline bool FIRST_constEnum(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_VariableDeclaration(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_designator(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_selector(int tt) {
	return tt == Tok_Lpar || tt == Tok_Lbrack || tt == Tok_Dot;
}

static inline bool FIRST_ExpList(int tt) {
	switch(tt){
	case Tok_hexstring:
	case Tok_NOT:
	case Tok_Tilde:
	case Tok_NIL:
	case Tok_Lbrace:
	case Tok_ident:
	case Tok_TRUE:
	case Tok_hexchar:
	case Tok_Plus:
	case Tok_integer:
	case Tok_string:
	case Tok_real:
	case Tok_FALSE:
	case Tok_Lpar:
	case Tok_Minus:
		return true;
	default: return false;
	}
}

static inline bool FIRST_expression(int tt) {
	switch(tt){
	case Tok_hexstring:
	case Tok_NOT:
	case Tok_Tilde:
	case Tok_NIL:
	case Tok_Lbrace:
	case Tok_ident:
	case Tok_TRUE:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_Plus:
	case Tok_string:
	case Tok_real:
	case Tok_FALSE:
	case Tok_Lpar:
	case Tok_Minus:
		return true;
	default: return false;
	}
}

static inline bool FIRST_relation(int tt) {
	switch(tt){
	case Tok_Lt:
	case Tok_Eq:
	case Tok_Gt:
	case Tok_Leq:
	case Tok_IS:
	case Tok_Hash:
	case Tok_IN:
	case Tok_Geq:
		return true;
	default: return false;
	}
}

static inline bool FIRST_SimpleExpression(int tt) {
	switch(tt){
	case Tok_hexstring:
	case Tok_NOT:
	case Tok_Tilde:
	case Tok_NIL:
	case Tok_Lbrace:
	case Tok_ident:
	case Tok_TRUE:
	case Tok_hexchar:
	case Tok_Plus:
	case Tok_integer:
	case Tok_string:
	case Tok_real:
	case Tok_FALSE:
	case Tok_Lpar:
	case Tok_Minus:
		return true;
	default: return false;
	}
}

static inline bool FIRST_AddOperator(int tt) {
	return tt == Tok_OR || tt == Tok_Plus || tt == Tok_Minus;
}

static inline bool FIRST_term(int tt) {
	switch(tt){
	case Tok_hexstring:
	case Tok_NOT:
	case Tok_Tilde:
	case Tok_NIL:
	case Tok_Lbrace:
	case Tok_ident:
	case Tok_TRUE:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_string:
	case Tok_real:
	case Tok_FALSE:
	case Tok_Lpar:
		return true;
	default: return false;
	}
}

static inline bool FIRST_MulOperator(int tt) {
	switch(tt){
	case Tok_Star:
	case Tok_MOD:
	case Tok_AND:
	case Tok_Amp:
	case Tok_Slash:
	case Tok_DIV:
		return true;
	default: return false;
	}
}

static inline bool FIRST_literal(int tt) {
	switch(tt){
	case Tok_NIL:
	case Tok_TRUE:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_string:
    case Tok_hexstring:
	case Tok_real:
	case Tok_FALSE:
		return true;
	default: return false;
	}
}

static inline bool FIRST_constructor(int tt) {
    return tt == Tok_Lbrace || tt == Tok_ident;
}

static inline bool FIRST_component(int tt) {
    switch(tt){
    case Tok_hexstring:
    case Tok_NOT:
    case Tok_Tilde:
    case Tok_NIL:
    case Tok_Lbrace:
    case Tok_ident:
    case Tok_TRUE:
    case Tok_hexchar:
    case Tok_integer:
    case Tok_Plus:
    case Tok_real:
    case Tok_FALSE:
    case Tok_string:
    case Tok_Lbrack:
    case Tok_Minus:
    case Tok_Lpar:
        return true;
    default: return false;
    }
}

static inline bool FIRST_factor(int tt) {
    switch(tt){
    case Tok_hexstring:
    case Tok_NOT:
    case Tok_Tilde:
    case Tok_NIL:
    case Tok_Lbrace:
    case Tok_ident:
    case Tok_TRUE:
    case Tok_hexchar:
    case Tok_integer:
    case Tok_real:
    case Tok_FALSE:
    case Tok_string:
    case Tok_Lpar:
        return true;
    default: return false;
    }
}

static inline bool FIRST_variableOrFunctionCall(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_statement(int tt) {
	switch(tt){
	case Tok_EXIT:
	case Tok_IF:
	case Tok_ident:
	case Tok_FOR:
	case Tok_LOOP:
	case Tok_WHILE:
	case Tok_RETURN:
	case Tok_CASE:
	case Tok_REPEAT:
		return true;
	default: return false;
	}
}

static inline bool FIRST_assignmentOrProcedureCall(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_StatementSequence(int tt) {
	switch(tt){
	case Tok_EXIT:
	case Tok_IF:
	case Tok_FOR:
	case Tok_ident:
	case Tok_LOOP:
	case Tok_WHILE:
	case Tok_RETURN:
	case Tok_CASE:
	case Tok_REPEAT:
		return true;
	default: return false;
	}
}

static inline bool FIRST_IfStatement(int tt) {
	return tt == Tok_IF;
}

static inline bool FIRST_ElsifStatement(int tt) {
	return tt == Tok_ELSIF;
}

static inline bool FIRST_ElseStatement(int tt) {
	return tt == Tok_ELSE;
}

static inline bool FIRST_CaseStatement(int tt) {
	return tt == Tok_CASE;
}

static inline bool FIRST_Case(int tt) {
	switch(tt){
	case Tok_hexstring:
	case Tok_NOT:
	case Tok_Tilde:
	case Tok_NIL:
	case Tok_Lbrace:
	case Tok_ident:
	case Tok_TRUE:
	case Tok_hexchar:
	case Tok_Plus:
	case Tok_integer:
	case Tok_string:
	case Tok_real:
	case Tok_FALSE:
	case Tok_Lpar:
	case Tok_Minus:
		return true;
	default: return false;
	}
}

static inline bool FIRST_LabelRange(int tt) {
	switch(tt){
	case Tok_hexstring:
	case Tok_NOT:
	case Tok_Tilde:
	case Tok_NIL:
	case Tok_Lbrace:
	case Tok_ident:
	case Tok_TRUE:
	case Tok_hexchar:
	case Tok_Plus:
	case Tok_integer:
	case Tok_string:
	case Tok_real:
	case Tok_FALSE:
	case Tok_Lpar:
	case Tok_Minus:
		return true;
	default: return false;
	}
}

static inline bool FIRST_label(int tt) {
	switch(tt){
	case Tok_hexstring:
	case Tok_NOT:
	case Tok_Tilde:
	case Tok_NIL:
	case Tok_Lbrace:
	case Tok_ident:
	case Tok_TRUE:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_Plus:
	case Tok_string:
	case Tok_real:
	case Tok_FALSE:
	case Tok_Lpar:
	case Tok_Minus:
		return true;
	default: return false;
	}
}

static inline bool FIRST_WhileStatement(int tt) {
	return tt == Tok_WHILE;
}

static inline bool FIRST_RepeatStatement(int tt) {
	return tt == Tok_REPEAT;
}

static inline bool FIRST_ForStatement(int tt) {
	return tt == Tok_FOR;
}

static inline bool FIRST_LoopStatement(int tt) {
	return tt == Tok_LOOP;
}

static inline bool FIRST_ExitStatement(int tt) {
	return tt == Tok_EXIT;
}

static inline bool FIRST_procedure(int tt) {
	return tt == Tok_PROC || tt == Tok_PROCEDURE;
}

static inline bool FIRST_ProcedureType(int tt) {
	return tt == Tok_PROC || tt == Tok_PROCEDURE;
}

static inline bool FIRST_ProcedureDeclaration(int tt) {
	return tt == Tok_PROC || tt == Tok_PROCEDURE;
}

static inline bool FIRST_ProcedureHeading(int tt) {
	return tt == Tok_PROC || tt == Tok_PROCEDURE;
}

static inline bool FIRST_Receiver(int tt) {
	return tt == Tok_Lpar;
}

static inline bool FIRST_block(int tt) {
	return tt == Tok_BEGIN;
}

static inline bool FIRST_ProcedureBody(int tt) {
	switch(tt){
	case Tok_PROC:
	case Tok_BEGIN:
	case Tok_PROCEDURE:
	case Tok_CONST:
	case Tok_VAR:
	case Tok_TYPE:
		return true;
	default: return false;
	}
}

static inline bool FIRST_DeclarationSequence(int tt) {
	return tt == Tok_PROC || tt == Tok_PROCEDURE || tt == Tok_CONST || tt == Tok_VAR || tt == Tok_TYPE;
}

static inline bool FIRST_ReturnStatement(int tt) {
	return tt == Tok_RETURN;
}

static inline bool FIRST_FormalParameters(int tt) {
	return tt == Tok_Lpar;
}

static inline bool FIRST_ReturnType(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_FPSection(int tt) {
	return tt == Tok_ident || tt == Tok_IN || tt == Tok_VAR;
}

static inline bool FIRST_FormalType(int tt) {
	switch(tt){
	case Tok_PROC:
	case Tok_RECORD:
	case Tok_HASHMAP:
	case Tok_ident:
	case Tok_ARRAY:
	case Tok_PROCEDURE:
	case Tok_Lbrack:
	case Tok_Lpar:
		return true;
	default: return false;
	}
}

static inline bool FIRST_module(int tt) {
	return tt == Tok_MODULE;
}

static inline bool FIRST_ImportList(int tt) {
	return tt == Tok_IMPORT;
}

static inline bool FIRST_import(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_MetaActuals(int tt) {
	return tt == Tok_Lpar;
}

static inline bool FIRST_MetaParams(int tt) {
	return tt == Tok_Lpar;
}

static inline bool FIRST_MetaSection(int tt) {
	return tt == Tok_ident || tt == Tok_CONST || tt == Tok_TYPE;
}

Parser2::~Parser2()
{
    if( thisMod )
    {
        clearTemps();
        Declaration::deleteAll(thisMod);
    }
}

void Parser2::RunParser(const MetaActualList& ma) {
	errors.clear();
	next();
    metaActuals = ma;
    Luon();
}

Parser2::Result Parser2::takeResult()
{
    Result res;
    res.first = thisMod;
    res.second = temporaries;
    thisMod = 0;
    temporaries.clear();
    return res;
}

void Parser2::next() {
	cur = la;
	la = scanner->next();
	while( la.d_type == Tok_Invalid ) {
        errors << Error(la.d_val, RowCol(la.d_lineNr, la.d_colNr), la.d_sourcePath);
		la = scanner->next();
	}
}

Token Parser2::peek(int off) {
	if( off == 1 )
		return la;
	else if( off == 0 )
		return cur;
	else
		return scanner->peek(off-1);
}

void Parser2::invalid(const char* what) {
    errors << Error(QString("invalid %1").arg(what),RowCol(la.d_lineNr, la.d_colNr), la.d_sourcePath);
}

bool Parser2::expect(int tt, bool pkw, const char* where) {
	if( la.d_type == tt) { next(); return true; }
    else { errors << Error(QString("'%1' expected in %2").arg(tokenTypeString(tt)).arg(where),
                           RowCol(la.d_lineNr, la.d_colNr), la.d_sourcePath); return false; }
}

static inline void dummy() {}

void Parser2::Luon() {
	module();
}

Expression* Parser2::number() {
    Expression* res = Expression::createFromToken(la.d_type,la.toRowCol());
    if( la.d_type == Tok_integer ) {
		expect(Tok_integer, false, "number");
        const Token tok = cur;
        QByteArray number = cur.d_val.toLower();
        number.replace('_',"");
        Type* type = 0;
        if( number.size() > 1 && number[number.size()-1] == 'l' )
        {
            number.chop(1);
            type = mdl->getType(BasicType::UINT64);
        }
        const char suffix = ( number.size() > 1 ? number[number.size()-1] : '0' );
        if( !::isdigit(suffix) )
            number.chop(1);
        switch(suffix)
        {
        case 'o':
            res->val = number.toULongLong(0,8);
            break;
        case 'b':
            res->val = number.toULongLong(0,2);
            break;
        case 'h':
            res->val = number.toULongLong(0,16);
            break;
        default:
            res->val = number.toULongLong();
            break;
        }
        if( type == 0 )
        {
            if( res->val.toULongLong() > 0xffffffff )
                type = mdl->getType(BasicType::UINT32);
            else
                type = mdl->getType(BasicType::UINT64);
        }
        if( res->val.toULongLong() > maxUlong )
            error(tok,QString("can only represent unsigned integers from 0 to %1").arg(maxUlong) );
        res->type = type;
    } else if( la.d_type == Tok_real ) {
		expect(Tok_real, false, "number");
        QByteArray str = cur.d_val;
        str.replace('_',"");
        res->type = mdl->getType(BasicType::LONGREAL);
        res->val = str.toDouble();
    } else
		invalid("number");
    return res;
}

Parser2::Quali Parser2::qualident() {
    Quali res;
    if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Dot )  ) {
        expect(Tok_ident, false, "qualident");
        res.first = cur.d_val;
        expect(Tok_Dot, false, "qualident");
    }
    expect(Tok_ident, false, "qualident");
    res.second = cur.d_val;
    return res;
}

Parser2::IdentDef Parser2::identdef() {
    IdentDef res;
    res.visi = IdentDef::Private;
    expect(Tok_ident, false, "identdef");
    res.name = cur;
    if( la.d_type == Tok_Star || la.d_type == Tok_Minus ) {
        if( la.d_type == Tok_Star ) {
            expect(Tok_Star, false, "identdef");
            res.visi = IdentDef::Public;
        } else if( la.d_type == Tok_Minus ) {
            expect(Tok_Minus, false, "identdef");
            res.visi = IdentDef::ReadOnly;
        } else
            invalid("identdef");
    }
    return res;
}

void Parser2::ConstDeclaration() {
    const IdentDef id = identdef();
    expect(Tok_Eq, false, "ConstDeclaration");
    Declaration* d = addDecl(id, Declaration::ConstDecl);
    if( d == 0 )
        return;
    d->expr = ConstExpression();
}

Expression* Parser2::ConstExpression() {
    return expression();
}

void Parser2::TypeDeclaration() {
    const IdentDef id = identdef();
    expect(Tok_Eq, false, "TypeDeclaration");
    Type* t = 0;

    // declare the type immediately so it is known in the forthcoming declaration
    Declaration* d = addDecl(id,Declaration::TypeDecl);
    if( d == 0 )
        return;
    thisDecl = d;
    if( FIRST_NamedType(la.d_type) ) {
        t = NamedType();
    }else
        t = type(false);
    if( t && t->decl == 0 && t->form != Type::NameRef )
    {
        t->decl = d;
        d->ownstype = true;
    }
    d->type = t;
    thisDecl = 0;
}

Type* Parser2::type(bool deanonymize) {
    bool isNewType = true;
    Type* res = 0;
	if( FIRST_NamedType(la.d_type) ) {
        res = NamedType();
        isNewType = false;
    } else if( FIRST_ArrayType(la.d_type) ) {
        res = ArrayType();
	} else if( FIRST_DictType(la.d_type) ) {
        res = DictType();
	} else if( FIRST_RecordType(la.d_type) ) {
        res = RecordType();
	} else if( FIRST_ProcedureType(la.d_type) ) {
        res = ProcedureType();
	} else if( FIRST_enumeration(la.d_type) ) {
        res = enumeration();
	} else
		invalid("type");
    if( res && res->form != BasicType::Undefined && isNewType && deanonymize )
        addHelper(res);
    return res;
}

Type* Parser2::NamedType() {
    Quali q = qualident();
    Type* res = new Type();
    res->form = Type::NameRef;
    res->expr = new Expression(Expression::NameRef, la.toRowCol());
    res->expr->val = QVariant::fromValue(q);
    temporaries.append(res);
    return res;
}

Type* Parser2::ArrayType() {
    Type* etype = 0;
    Expression* len = 0;
	if( la.d_type == Tok_ARRAY ) {
		expect(Tok_ARRAY, true, "ArrayType");
		if( FIRST_length(la.d_type) ) {
            len = length();
		}
		expect(Tok_OF, true, "ArrayType");
        etype = type();
	} else if( la.d_type == Tok_Lbrack ) {
		expect(Tok_Lbrack, false, "ArrayType");
		if( FIRST_length(la.d_type) ) {
            len = length();
		}
		expect(Tok_Rbrack, false, "ArrayType");
        etype = type();
	} else
		invalid("ArrayType");

    Type* arr = new Type();
    arr->form = Type::Array;
    arr->expr = len;
    arr->base = etype;
    return arr;
}

Expression* Parser2::length() {
    return ConstExpression();
}

Type* Parser2::DictType() {
	expect(Tok_HASHMAP, true, "DictType");
    Type* key = NamedType();
	expect(Tok_OF, true, "DictType");
    Type* etype = type();
    Type* dict = new Type();
    dict->form = Type::HashMap;
    dict->expr = key->expr;
    key->expr = 0;
    delete key;
    dict->base = etype;
    return dict;
}

Type* Parser2::RecordType() {
	expect(Tok_RECORD, true, "RecordType");
    Type* rec = new Type();
    rec->form = Type::Record;
    mdl->openScope(0);
    if( la.d_type == Tok_Lpar ) {
		expect(Tok_Lpar, false, "RecordType");
        rec->base = BaseType();
		expect(Tok_Rpar, false, "RecordType");
	}
	while( FIRST_FieldList(la.d_type) ) {
		FieldList();
		if( la.d_type == Tok_Semi ) {
			expect(Tok_Semi, false, "RecordType");
		}
	}
    rec->subs = AstModel::toList(mdl->closeScope(true));
    expect(Tok_END, true, "RecordType");
    return rec;
}

Type* Parser2::BaseType() {
    return NamedType();
}

void Parser2::FieldList() {
    const IdentDefList l = IdentList();
    expect(Tok_Colon, false, "FieldList");
    Type* t = type();
    for(int i = 0; i < l.size(); i++ )
    {
        Declaration* d = addDecl(l[i],Declaration::Field);
        if( d == 0 )
            continue;
        d->type = t;
    }
}

Parser2::IdentDefList Parser2::IdentList() {
    IdentDefList res;
    res << identdef();
    while( la.d_type == Tok_Comma || FIRST_identdef(la.d_type) ) {
        if( la.d_type == Tok_Comma ) {
            expect(Tok_Comma, false, "IdentList");
        }
        res << identdef();
    }
    return res;
}

Type* Parser2::enumeration() {
    expect(Tok_Lpar, false, "enumeration");
    Type* res = new Type();
    if( FIRST_constEnum(la.d_type) ) {
        res->subs = constEnum();
        foreach( Declaration* d, res->subs )
            d->type = res;
        res->form = Type::ConstEnum;
    } else
        invalid("enumeration");
    expect(Tok_Rpar, false, "enumeration");

    return res;
}

DeclList Parser2::constEnum() {
    expect(Tok_ident, false, "constEnum");

    DeclList res;

    Declaration* d = addDecl(cur, 0, Declaration::ConstDecl);
    if( d == 0 )
        return res;

    if( la.d_type == Tok_Eq ) {
        expect(Tok_Eq, false, "constEnum");
        d->expr = ConstExpression();
    }

    res << d;

    while( la.d_type == Tok_Comma || la.d_type == Tok_ident ) {
        if( la.d_type == Tok_Comma ) {
            expect(Tok_Comma, false, "constEnum");
        }
        expect(Tok_ident, false, "constEnum");
        d = addDecl(cur, 0, Declaration::ConstDecl);
        if( d == 0 )
            continue;
        res << d;
    }
    return res;
}

void Parser2::VariableDeclaration() {
    const IdentDefList ids = IdentList();
    expect(Tok_Colon, false, "VariableDeclaration");
    Type* t = type();
    if( t == 0 )
        return;
    Declaration* outer = mdl->getTopScope();
    foreach( const IdentDef& id, ids )
    {
        Declaration* d = addDecl(id,outer->mode == Declaration::Module ?
                                     Declaration::VarDecl : Declaration::LocalDecl);
        if( d == 0 )
            continue;
        d->outer = outer;
        d->type = t;
    }
}

// designator results in an lvalue if possible, unless needsLvalue is false
Expression* Parser2::designator(bool needsLvalue) {

    Quali q = qualident();
    Expression* res = new Expression(Expression::NameRef, cur.toRowCol());
    res->val = QVariant::fromValue(q);

    while( FIRST_selector(la.d_type) ) {
        // inlined selector

        if( la.d_type == Tok_Dot ) {
            expect(Tok_Dot, false, "selector");
            expect(Tok_ident, false, "selector");
            Expression* tmp = new Expression(Expression::Select, cur.toRowCol() );
            tmp->val = QVariant::fromValue(cur.d_val);
            tmp->lhs = res;
            res = tmp;
        } else if( la.d_type == Tok_Lbrack ) {
            expect(Tok_Lbrack, false, "selector");
            Expression* tmp = new Expression(Expression::Index, cur.toRowCol() );
            tmp->lhs = res;
            res = tmp;
            res->rhs = expression();
            if( res->rhs == 0 )
            {
                delete res;
                return 0;
            }
            expect(Tok_Rbrack, false, "selector");
        } else if( la.d_type == Tok_Lpar ) {
            expect(Tok_Lpar, false, "selector");
            const Token lpar = cur;
            Expression* args = 0;
            if( FIRST_expression(la.d_type) ) {
                // inlined ExpList
                args = expression(true);
                if( args == 0 )
                {
                    delete res;
                    return 0;
                }
                while( la.d_type == Tok_Comma || FIRST_expression(la.d_type) ) {
                    if( la.d_type == Tok_Comma )
                        expect(Tok_Comma, false, "ExpList");
                    Expression* arg = expression(true);
                    if( arg == 0 )
                    {
                        delete res;
                        return 0;
                    }
                    Expression::appendArg(args,arg);
                }
            }
            expect(Tok_Rpar, false, "selector");

            if( res->kind == Expression::Builtin && res->val.toInt() == Builtin::ASSERT )
            {
                Expression* e = new Expression(Expression::Literal,lpar.toRowCol());
                e->type = mdl->getType(BasicType::UINT32);
                e->val = lpar.d_lineNr;
                Expression::appendArg(args,e);
                e = new Expression(Expression::Literal,lpar.toRowCol());
                e->type = mdl->getType(BasicType::StrLit);
                e->val = "\"" + lpar.d_sourcePath.toUtf8() + "\"";
                Expression::appendArg(args,e);
            }

            Expression* tmp = new Expression(Expression::Call, lpar.toRowCol() ); // could be call or typecast at this point
            tmp->lhs = res; // proc
            tmp->rhs = args;
            res = tmp;
        } else
            invalid("selector");
    }

    res->needsLval = needsLvalue;
    return res;
}

Expression* Parser2::expression(bool lvalue) {
    Expression* res = SimpleExpression(lvalue);
    if( res == 0 )
        return 0;
    if( FIRST_relation(la.d_type) ) {
        const Token tok = la;
        Expression* tmp = Expression::createFromToken(relation(), tok.toRowCol());
        tmp->lhs = res;
        tmp->type = mdl->getType(BasicType::BOOLEAN);
        res = tmp;
        res->rhs = SimpleExpression(false);
        if( res->rhs == 0 )
        {
            delete tmp;
            return 0;
        }
    }
    return res;
}

quint8 Parser2::relation() {
	if( la.d_type == Tok_Eq ) {
		expect(Tok_Eq, false, "relation");
	} else if( la.d_type == Tok_Hash ) {
		expect(Tok_Hash, false, "relation");
	} else if( la.d_type == Tok_Lt ) {
		expect(Tok_Lt, false, "relation");
	} else if( la.d_type == Tok_Leq ) {
		expect(Tok_Leq, false, "relation");
	} else if( la.d_type == Tok_Gt ) {
		expect(Tok_Gt, false, "relation");
	} else if( la.d_type == Tok_Geq ) {
		expect(Tok_Geq, false, "relation");
	} else if( la.d_type == Tok_IN ) {
		expect(Tok_IN, false, "relation");
	} else if( la.d_type == Tok_IS ) {
		expect(Tok_IS, true, "relation");
	} else
		invalid("relation");
    return cur.d_type;
}

Expression* Parser2::SimpleExpression(bool lvalue) {
    quint8 op = 0;
    Token tok = la;
    if( la.d_type == Tok_Plus || la.d_type == Tok_Minus ) {
        if( la.d_type == Tok_Plus ) {
            expect(Tok_Plus, false, "SimpleExpression");
            op = Tok_Plus;
        } else if( la.d_type == Tok_Minus ) {
            expect(Tok_Minus, false, "SimpleExpression");
            op = Tok_Minus;
        } else
            invalid("SimpleExpression");
    }
    Expression* res = term(lvalue);
    if( res == 0 )
        return 0;
    if( op != 0 ) {
        Expression* tmp = new Expression(op == Tok_Plus ? Expression::Plus : Expression::Minus, tok.toRowCol());
        tmp->lhs = res;
        tmp->type = res->type;
        res = tmp;
    }
    while( FIRST_AddOperator(la.d_type) ) {
        Token tok = la;
        Expression* tmp = Expression::createFromToken(AddOperator(), tok.toRowCol());
        tmp->lhs = res;
        res = tmp;
        res->rhs = term(false);
        if( res->rhs == 0 )
        {
            delete tmp;
            return 0;
        }
    }
    return res;
}

quint8 Parser2::AddOperator() {
	if( la.d_type == Tok_Plus ) {
		expect(Tok_Plus, false, "AddOperator");
	} else if( la.d_type == Tok_Minus ) {
		expect(Tok_Minus, false, "AddOperator");
	} else if( la.d_type == Tok_OR ) {
		expect(Tok_OR, true, "AddOperator");
	} else
		invalid("AddOperator");
    return cur.d_type;
}

Expression* Parser2::term(bool lvalue) {
    Expression* res = factor(lvalue);
    if( res == 0 )
        return 0;
    while( FIRST_MulOperator(la.d_type) ) {
        Token tok = la;
        Expression* tmp = Expression::createFromToken(MulOperator(),tok.toRowCol());
        tmp->lhs = res;
        res = tmp;
        res->rhs = factor();
        if( res->rhs == 0 )
            return 0;
    }
    return res;
}

quint8 Parser2::MulOperator() {
	if( la.d_type == Tok_Star ) {
		expect(Tok_Star, false, "MulOperator");
	} else if( la.d_type == Tok_Slash ) {
		expect(Tok_Slash, false, "MulOperator");
	} else if( la.d_type == Tok_DIV ) {
		expect(Tok_DIV, true, "MulOperator");
	} else if( la.d_type == Tok_MOD ) {
		expect(Tok_MOD, true, "MulOperator");
	} else if( la.d_type == Tok_Amp ) {
		expect(Tok_Amp, false, "MulOperator");
	} else if( la.d_type == Tok_AND ) {
		expect(Tok_AND, true, "MulOperator");
	} else
		invalid("MulOperator");
    return cur.d_type;
}

static QByteArray dequote(const QByteArray& str)
{
    QByteArray res;
    if( str.startsWith('\'') && str.endsWith('\'') ||
            str.startsWith('"') && str.endsWith('"') )
        res = str.mid(1,str.size()-2);
    else
        res = str;
    res += '\0'; // make terminating zero explicit in value
    return res;
}

Expression* Parser2::literal() {
    Expression* res;
    if( FIRST_number(la.d_type) ) {
        res = number();
	} else if( la.d_type == Tok_string ) {
		expect(Tok_string, false, "literal");
        res = new Expression(Expression::Literal,cur.toRowCol());
        res->type = mdl->getType(BasicType::StrLit);
        res->val = dequote(cur.d_val);
        // string literal: byte array latin-1 with type BasicType::String
    } else if( la.d_type == Tok_hexstring ) {
        expect(Tok_hexstring, false, "literal");
        // alternative syntax for A{ x x x } with A = array of byte
        res = new Expression(Expression::Literal,cur.toRowCol());
        const QByteArray bytes = QByteArray::fromHex(cur.d_val); // already comes without quotes
        res->type = mdl->getType(BasicType::ByteArrayLit);
        res->val = bytes;
        // byte array literal: byte array with type array of uint8
    } else if( la.d_type == Tok_hexchar ) {
        expect(Tok_hexchar, false, "literal");
        res = new Expression(Expression::Literal,cur.toRowCol());
        res->type = mdl->getType(BasicType::CHAR);
        QByteArray tmp = cur.d_val;
        tmp.chop(1); // remove X postfix
        res->val = QVariant::fromValue((char)(quint8)tmp.toUInt(0,16));
    } else if( la.d_type == Tok_NIL ) {
        expect(Tok_NIL, true, "literal");
        res = new Expression(Expression::Literal,cur.toRowCol());
        res->type = mdl->getType(BasicType::Nil);
        res->val = QVariant();
    } else if( la.d_type == Tok_TRUE ) {
        expect(Tok_TRUE, true, "literal");
        res = new Expression(Expression::Literal,cur.toRowCol());
        res->type = mdl->getType(BasicType::BOOLEAN);
        res->val = true;
    } else if( la.d_type == Tok_FALSE ) {
        expect(Tok_FALSE, true, "literal");
        res = new Expression(Expression::Literal,cur.toRowCol());
        res->type = mdl->getType(BasicType::BOOLEAN);
        res->val = false;
    } else
		invalid("literal");
    return res;
}

Expression* Parser2::constructor() {
    Expression* res = new Expression();
    if( FIRST_NamedType(la.d_type) ) {
        NamedType();
    }
    expect(Tok_Lbrace, false, "constructor");
    if( FIRST_component(la.d_type) ) {
        component();
        while( la.d_type == Tok_Comma || FIRST_component(la.d_type) ) {
            if( la.d_type == Tok_Comma ) {
                expect(Tok_Comma, false, "constructor");
            }
            component();
        }
    }
    expect(Tok_Rbrace, false, "constructor");
    return res;
}

Expression* Parser2::component() {
    Expression* res;
    if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Colon )  ) {
        expect(Tok_ident, false, "component");
        Quali q;
        q.second = cur.d_val;
        expect(Tok_Colon, false, "component");
        const Token t = cur;
        Expression* rhs = expression();
        if( rhs == 0 )
            return 0;
        res = new Expression(Expression::KeyValue, t.toRowCol());
        res->lhs = new Expression(Expression::NameRef);
        res->lhs->val = QVariant::fromValue(q);
        res->rhs = rhs;
    } else if( la.d_type == Tok_Lbrack ) {
        expect(Tok_Lbrack, false, "component");
        Expression* lhs = expression();
        if( lhs == 0 )
            return 0;
        expect(Tok_Rbrack, false, "component");
        expect(Tok_Colon, false, "component");
        const Token t = cur;
        Expression* rhs = expression();
        if( rhs == 0 )
        {
            delete lhs;
            return 0;
        }
        res = new Expression(Expression::KeyValue, t.toRowCol());
        res->lhs = new Expression(Expression::Index, lhs->pos);
        res->lhs->rhs = lhs; // special case of Index with no lhs
        res->rhs = rhs;
    } else if( FIRST_expression(la.d_type) ) {
        res = expression();
        if( res == 0 )
            return 0;
        if( la.d_type == Tok_2Dot ) {
            expect(Tok_2Dot, false, "component");
            const Token t = cur;
            Expression* rhs = expression();
            if( rhs == 0 )
            {
                delete res;
                return 0;
            }
            Expression* range = new Expression(Expression::Range, t.toRowCol());
            range->lhs = res;
            range->rhs = rhs;
            res = range;
        }
    } else
        invalid("component");
    return res;
}

Expression* Parser2::factor(bool lvalue) {
    Expression* res = 0;
    if( ( ( peek(1).d_type == Tok_Lbrace || peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Lbrace ) )  ) {
        res = constructor();
	} else if( FIRST_literal(la.d_type) ) {
        res = literal();
	} else if( FIRST_variableOrFunctionCall(la.d_type) ) {
        res = variableOrFunctionCall(lvalue);
	} else if( la.d_type == Tok_Lpar ) {
		expect(Tok_Lpar, false, "factor");
        res = expression();
        expect(Tok_Rpar, false, "factor");
	} else if( la.d_type == Tok_Tilde || la.d_type == Tok_NOT ) {
		if( la.d_type == Tok_Tilde ) {
			expect(Tok_Tilde, false, "factor");
		} else if( la.d_type == Tok_NOT ) {
			expect(Tok_NOT, true, "factor");
		} else
			invalid("factor");
        Expression* tmp = factor();
        if( tmp == 0 )
            return 0;

        res = new Expression(Expression::Not, cur.toRowCol());
        res->lhs = tmp;
    } else
		invalid("factor");
    return res;
}

Expression* Parser2::variableOrFunctionCall(bool lvalue) {
    return designator(lvalue);
}

Statement* Parser2::statement() {
    Statement* res = 0;
	if( FIRST_assignmentOrProcedureCall(la.d_type) ) {
        res = assignmentOrProcedureCall();
	} else if( FIRST_IfStatement(la.d_type) ) {
        res = IfStatement();
	} else if( FIRST_CaseStatement(la.d_type) ) {
        res = CaseStatement();
	} else if( FIRST_LoopStatement(la.d_type) ) {
        res = LoopStatement();
	} else if( FIRST_ExitStatement(la.d_type) ) {
        res = ExitStatement();
	} else if( FIRST_ReturnStatement(la.d_type) ) {
        res = ReturnStatement();
	} else if( FIRST_WhileStatement(la.d_type) ) {
        res = WhileStatement();
	} else if( FIRST_RepeatStatement(la.d_type) ) {
        res = RepeatStatement();
	} else if( FIRST_ForStatement(la.d_type) ) {
        res = ForStatement();
	} else
		invalid("statement");
    return res;
}

Statement* Parser2::assignmentOrProcedureCall() {
    Token t = la;
    if( t.d_lineNr == 25 )
        dummy();
    Expression* lhs = designator(true);
    if( lhs == 0 )
        return 0;
    if( la.d_type == Tok_ColonEq ) {
        expect(Tok_ColonEq, false, "assignmentOrProcedureCall");
        const Token t = cur;
        Expression* rhs = expression();
        if( rhs == 0 )
        {
            delete lhs;
            return 0;
        }
        Statement* stat = new Statement(Statement::Assig, t.toRowCol());
        stat->lhs = lhs;
        stat->rhs = rhs;
        return stat;
    }else
    {
        // TODO
        // call procedure without ()
        Statement* stat = new Statement(Statement::Call, t.toRowCol());
        stat->lhs = lhs;
        return stat;
    }
}

Statement* Parser2::StatementSequence() {
    Statement* first = 0;
    Statement* last = 0;
	while( FIRST_statement(la.d_type) ) {
        Statement* stat = statement();
        if( stat == 0 )
        {
            Statement::deleteAll(first);
            return 0;
        }
        if( last )
            last->append(stat);
        last = stat->getLast();
        if( first == 0 )
            first = stat;
		while( la.d_type == Tok_Semi ) {
			expect(Tok_Semi, false, "StatementSequence");
		}
	}
    return first;
}

Statement* Parser2::IfStatement() {
	expect(Tok_IF, true, "IfStatement");
    Statement* first = new Statement(Statement::If,cur.toRowCol());
    first->rhs = expression();
    if( first->rhs == 0 )
    {
        Statement::deleteAll(first);
        return 0;
    }
	expect(Tok_THEN, true, "IfStatement");
    first->body = StatementSequence();
    Statement* last = first;
	while( FIRST_ElsifStatement(la.d_type) ) {
        Statement* stat = ElsifStatement();
        if( stat == 0 )
        {
            Statement::deleteAll(first);
            return 0;
        }
        last->append(stat);
        last = stat;
	}
	if( FIRST_ElseStatement(la.d_type) ) {
        Statement* stat = ElseStatement();
        if( stat == 0 )
        {
            Statement::deleteAll(first);
            return 0;
        }
        last->append(stat);
    }
	expect(Tok_END, true, "IfStatement");
    return first;
}

Statement* Parser2::ElsifStatement() {
	expect(Tok_ELSIF, true, "ElsifStatement");
    Statement* res = new Statement(Statement::Elsif,cur.toRowCol());
    res->rhs = expression();
    if( res->rhs == 0 )
    {
        Statement::deleteAll(res);
        return 0;
    }
	expect(Tok_THEN, true, "ElsifStatement");
    res->body = StatementSequence();
    if( res->body == 0 )
    {
        Statement::deleteAll(res);
        return 0;
    }
    return res;
}

Statement* Parser2::ElseStatement() {
	expect(Tok_ELSE, true, "ElseStatement");
    Statement* res = new Statement(Statement::Else, cur.toRowCol());
    res->body = StatementSequence();
    if( res->body == 0 )
    {
        Statement::deleteAll(res);
        return 0;
    }
    return res;
}

Statement* Parser2::CaseStatement() {
	expect(Tok_CASE, true, "CaseStatement");
    Statement* first = new Statement(Statement::Case, cur.toRowCol());
    first->rhs = expression();
    if( first->rhs == 0 )
    {
        Statement::deleteAll(first);
        return 0;
    }
	expect(Tok_OF, true, "CaseStatement");
    Statement* last = first;
	if( FIRST_Case(la.d_type) ) {
        Statement* stat = Case();
        if( stat == 0 )
        {
            Statement::deleteAll(first);
            return 0;
        }
        last->append(stat);
        last = stat;
	}
	while( la.d_type == Tok_Bar ) {
		expect(Tok_Bar, false, "CaseStatement");
        Statement* stat = Case();
        if( stat == 0 )
        {
            Statement::deleteAll(first);
            return 0;
        }
        last->append(stat);
        last = stat;
    }
	if( la.d_type == Tok_ELSE ) {
		expect(Tok_ELSE, true, "CaseStatement");
        Statement* stat = new Statement(Statement::Else, cur.toRowCol());
        last->append(stat);
        last = stat;
        stat->body = StatementSequence();
        if( stat->body == 0 )
        {
            Statement::deleteAll(first);
            return 0;
        }
    }
	expect(Tok_END, true, "CaseStatement");
    return first;
}

Statement* Parser2::Case() {
    Statement* res = new Statement(Statement::CaseLabel, la.toRowCol());
    res->rhs = CaseLabelList();
    if( res->rhs == 0 )
    {
        Statement::deleteAll(res);
        return 0;
    }
    expect(Tok_Colon, false, "Case");
    res->body = StatementSequence(); // body may be empty
    return res;
}

Expression* Parser2::CaseLabelList() {
    Expression* first = LabelRange();
    if( first == 0 )
        return 0;
    Expression* last = first;
	while( la.d_type == Tok_Comma || FIRST_LabelRange(la.d_type) ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "CaseLabelList");
		}
        Expression* e = LabelRange();
        if( e == 0 )
        {
            delete first;
            return 0;
        }
        last->next = e;
        last = e;
	}
    return first;
}

Expression* Parser2::LabelRange() {
    Expression* lhs = label();
    if( lhs == 0 )
        return 0;
	if( la.d_type == Tok_2Dot ) {
		expect(Tok_2Dot, false, "LabelRange");
        const Token t = cur;
        Expression* rhs = label();
        if( rhs == 0 )
        {
            delete lhs;
            return 0;
        }
        Expression* tmp = new Expression(Expression::Range, t.toRowCol());
        tmp->lhs = lhs;
        tmp->rhs = rhs;
        lhs = tmp;
	}
    return lhs;
}

Expression* Parser2::label() {
    return ConstExpression();
}

Statement* Parser2::WhileStatement() {
	expect(Tok_WHILE, true, "WhileStatement");
    Statement* res = new Statement(Statement::While, cur.toRowCol());
    res->rhs = expression();
    if( res->rhs == 0 )
    {
        Statement::deleteAll(res);
        return 0;
    }
	expect(Tok_DO, true, "WhileStatement");
    res->body = StatementSequence();
	expect(Tok_END, true, "WhileStatement");
    return res;
}

Statement* Parser2::RepeatStatement() {
	expect(Tok_REPEAT, true, "RepeatStatement");
    Statement* res = new Statement(Statement::Repeat, cur.toRowCol());
    res->body = StatementSequence();
	expect(Tok_UNTIL, true, "RepeatStatement");
    res->rhs = expression();
    if( res->rhs == 0 )
    {
        Statement::deleteAll(res);
        return 0;
    }
    return res;
}

Statement* Parser2::ForStatement() {
	expect(Tok_FOR, true, "ForStatement");
    Statement* res = new Statement(Statement::ForAssig, cur.toRowCol());
    expect(Tok_ident, false, "ForStatement");
    res->lhs = new Expression(Expression::NameRef, cur.toRowCol() );
    res->lhs->val = QVariant::fromValue(Quali(QByteArray(),cur.d_val));
	expect(Tok_ColonEq, false, "ForStatement");
    res->rhs = expression();
    if(res->rhs == 0 )
    {
        Statement::deleteAll(res);
        return 0;
    }
	expect(Tok_TO, true, "ForStatement");
    Statement* forby = new Statement(Statement::ForToBy, cur.toRowCol());
    res->append(forby);
    forby->lhs = expression();
    if(forby->lhs == 0 )
    {
        Statement::deleteAll(res);
        return 0;
    }
    if( la.d_type == Tok_BY ) {
		expect(Tok_BY, true, "ForStatement");
        forby->rhs = ConstExpression();
	}
	expect(Tok_DO, true, "ForStatement");
    res->body = StatementSequence();
	expect(Tok_END, true, "ForStatement");
    return res;
}

Statement* Parser2::LoopStatement() {
	expect(Tok_LOOP, true, "LoopStatement");
    Statement* res = new Statement(Statement::Loop, cur.toRowCol());
    res->body = StatementSequence();
	expect(Tok_END, true, "LoopStatement");
    return res;
}

Statement* Parser2::ExitStatement() {
	expect(Tok_EXIT, true, "ExitStatement");
    return new Statement(Statement::Exit, cur.toRowCol());
}

void Parser2::procedure() {
	if( la.d_type == Tok_PROCEDURE ) {
		expect(Tok_PROCEDURE, true, "procedure");
	} else if( la.d_type == Tok_PROC ) {
		expect(Tok_PROC, true, "procedure");
	} else
		invalid("procedure");
}

Type* Parser2::ProcedureType() {
	procedure();
	if( la.d_type == Tok_Hat ) {
		expect(Tok_Hat, false, "ProcedureType");
	}
    Type* res = mdl->getType(BasicType::NoType);
	if( FIRST_FormalParameters(la.d_type) ) {
        res = FormalParameters();
	}
    return res;
}

void Parser2::ProcedureDeclaration() {
    procedure();

    Quali receiver;
    const Token t = la;
    Declaration* record = 0;
    if( FIRST_Receiver(la.d_type) ) {
        receiver = Receiver();
        record = mdl->findDecl(receiver.second);
        if( record == 0 || record->mode != Declaration::TypeDecl ||
                record->type == 0 || record->type->form != Type::Record )
        {
            error(t, "receiver must be a record type declaration");
            record = 0;
        }else
            mdl->openScope(0);
    }

    const IdentDef id = identdef();
    if( !id.isValid() )
        return; // invalid syntax

    Declaration* procDecl = addDecl(id, Declaration::Procedure);
    if( procDecl == 0 )
        return;

    mdl->openScope(procDecl);

    if( !receiver.first.isEmpty() )
    {
        Declaration* d = mdl->addDecl(receiver.first);
        d->mode = Declaration::ParamDecl;
        d->pos = t.toRowCol();
        d->receiver = 1;
        d->type = new Type();
        d->ownstype = true;
        d->type->form = Type::NameRef;
        d->type->expr = new Expression(Expression::NameRef,t.toRowCol());
        receiver.first.clear();
        d->type->expr->val = QVariant::fromValue(receiver);
        procDecl->receiver = 1;
    }

    if( FIRST_FormalParameters(la.d_type) ) {
        procDecl->type = FormalParameters();
    }

    if( la.d_type == Tok_INLINE || la.d_type == Tok_INVAR ) {
        if( la.d_type == Tok_INLINE ) {
            expect(Tok_INLINE, true, "ProcedureDeclaration");
            procDecl->inline_ = true;
        } else if( la.d_type == Tok_INVAR ) {
            expect(Tok_INVAR, true, "ProcedureDeclaration");
            procDecl->invar = true;
        } else
            invalid("ProcedureDeclaration");
    }
    if( la.d_type == Tok_Semi ) {
        expect(Tok_Semi, false, "ProcedureDeclaration");
    }
    if( FIRST_ProcedureBody(la.d_type) ) {
        // inlined ProcedureBody();
        DeclarationSequence();
        procDecl->body = block();
        expect(Tok_END, true, "ProcedureBody");
        expect(Tok_ident, false, "ProcedureBody");
        if( procDecl->name.constData() != cur.d_val.constData() )
            error(cur, QString("name after END differs from procedure name") );
    } else if( la.d_type == Tok_END ) {
        expect(Tok_END, true, "ProcedureDeclaration");
    } else
        invalid("ProcedureDeclaration");

    mdl->closeScope();
    if( record )
    {
        DeclList l = AstModel::toList(mdl->closeScope(true));
        for(int i = 0; i < l.size(); i++ )
        {
            if( record->type->findField(l[i]->name) )
                error(l[i]->pos, "name not unique in receiver");
        }
        record->type->subs += l;
    }
}

Parser2::Quali Parser2::Receiver() {
    Quali res;
	expect(Tok_Lpar, false, "Receiver");
	expect(Tok_ident, false, "Receiver");
    res.first = cur.d_val;
	expect(Tok_Colon, false, "Receiver");
	expect(Tok_ident, false, "Receiver");
    res.second = cur.d_val;
	expect(Tok_Rpar, false, "Receiver");
    return res;
}

Statement* Parser2::block() {
    expect(Tok_BEGIN, true, "block");
    return StatementSequence();
}

void Parser2::DeclarationSequence() {
	while( la.d_type == Tok_CONST || la.d_type == Tok_TYPE || la.d_type == Tok_VAR || FIRST_ProcedureDeclaration(la.d_type) ) {
		if( la.d_type == Tok_CONST ) {
			expect(Tok_CONST, true, "DeclarationSequence");
			while( FIRST_ConstDeclaration(la.d_type) ) {
				ConstDeclaration();
				if( la.d_type == Tok_Semi ) {
					expect(Tok_Semi, false, "DeclarationSequence");
				}
			}
		} else if( la.d_type == Tok_TYPE ) {
			expect(Tok_TYPE, true, "DeclarationSequence");
			while( FIRST_TypeDeclaration(la.d_type) ) {
				TypeDeclaration();
				if( la.d_type == Tok_Semi ) {
					expect(Tok_Semi, false, "DeclarationSequence");
				}
			}
		} else if( la.d_type == Tok_VAR ) {
			expect(Tok_VAR, true, "DeclarationSequence");
			while( FIRST_VariableDeclaration(la.d_type) ) {
				VariableDeclaration();
				if( la.d_type == Tok_Semi ) {
					expect(Tok_Semi, false, "DeclarationSequence");
				}
			}
		} else if( FIRST_ProcedureDeclaration(la.d_type) ) {
			ProcedureDeclaration();
			if( la.d_type == Tok_Semi ) {
				expect(Tok_Semi, false, "DeclarationSequence");
			}
		} else
			invalid("DeclarationSequence");
	}
}

Statement* Parser2::ReturnStatement() {
	expect(Tok_RETURN, true, "ReturnStatement");
    Statement* res = new Statement(Statement::Return, cur.toRowCol());
	if( FIRST_expression(la.d_type) ) {
        res->rhs = expression();
	}
    return res;
}

Type* Parser2::FormalParameters() {
	expect(Tok_Lpar, false, "FormalParameters");
	if( FIRST_FPSection(la.d_type) ) {
		FPSection();
        while( ( ( ( peek(1).d_type == Tok_ident || peek(1).d_type == Tok_IN ||
                     peek(1).d_type == Tok_VAR ) ||
                   peek(1).d_type == Tok_Semi && ( peek(2).d_type == Tok_ident ||
                                                   peek(2).d_type == Tok_IN || peek(2).d_type == Tok_VAR ) ) )  ) {
			if( la.d_type == Tok_Semi ) {
				expect(Tok_Semi, false, "FormalParameters");
			}
			FPSection();
		}
		if( la.d_type == Tok_Semi || la.d_type == Tok_2Dot ) {
			if( la.d_type == Tok_Semi ) {
				expect(Tok_Semi, false, "FormalParameters");
			}
			expect(Tok_2Dot, false, "FormalParameters");
            cur.d_val = Token::getSymbol("..");
            Declaration* d = addDecl(cur,0,Declaration::ParamDecl);
            Q_ASSERT(d);
            d->outer = mdl->getTopScope();
            d->type = mdl->getType(BasicType::NoType);
        }
	}
	expect(Tok_Rpar, false, "FormalParameters");
    Type* res = 0;
    if( la.d_type == Tok_Colon ) {
		expect(Tok_Colon, false, "FormalParameters");
        res = ReturnType();
	}
    if(res == 0)
        res = mdl->getType(BasicType::NoType);
    return res;
}

Type* Parser2::ReturnType() {
    return NamedType();
}

void Parser2::FPSection() {
    Declaration::Access a = Declaration::ByValue;
	if( la.d_type == Tok_VAR || la.d_type == Tok_IN ) {
		if( la.d_type == Tok_VAR ) {
			expect(Tok_VAR, false, "FPSection");
            a = Declaration::VAR;
		} else if( la.d_type == Tok_IN ) {
			expect(Tok_IN, false, "FPSection");
            a = Declaration::IN;
		} else
			invalid("FPSection");
	}
	expect(Tok_ident, false, "FPSection");
    TokenList l;
    l << cur;
    while( la.d_type == Tok_Comma || la.d_type == Tok_ident ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "FPSection");
		}
		expect(Tok_ident, false, "FPSection");
        l << cur;
    }
	expect(Tok_Colon, false, "FPSection");
    Type* t = FormalType();
    for(int i = 0; i < l.size(); i++ )
    {
        Declaration* d = addDecl(l[i], 0, Declaration::ParamDecl);
        if( d == 0 )
            continue;
        d->access = a;
        d->type = t;
    }
}

Type* Parser2::FormalType() {
    return type();
}

void Parser2::module() {
    if( la.d_type != Tok_MODULE )
    {
        la.d_sourcePath = scanner->source();
        la.d_lineNr = 1;
        error(la,"not a Luon module");
        return;
    }
    Declaration* m = new Declaration();
    m->mode = Declaration::Module;
    if( thisMod )
    {
        clearTemps();
        Declaration::deleteAll(thisMod);
    }
    thisMod = m;
    mdl->openScope(m);

    expect(Tok_MODULE, true, "module");
    expect(Tok_ident, false, "module");
    m->name = cur.d_val;

    ModuleData md;
    md.path = scanner->path();
    md.source = scanner->source();
    md.path += cur.d_val;
    if( imp )
        md.fullName = Token::getSymbol(imp->modulePath(md.path));
    else
        md.fullName = Token::getSymbol(md.path.join('/'));
    md.metaActuals = metaActuals;

    if( FIRST_MetaParams(la.d_type) ) {
        md.metaParams = MetaParams();
    }

    m->data = QVariant::fromValue(md);

    if( la.d_type == Tok_Semi ) {
        expect(Tok_Semi, false, "module");
    }
    while( FIRST_ImportList(la.d_type) || FIRST_DeclarationSequence(la.d_type) ) {
        if( FIRST_ImportList(la.d_type) ) {
            ImportList();
        } else if( FIRST_DeclarationSequence(la.d_type) || la.d_type == Tok_PROCEDURE || la.d_type == Tok_TYPE || la.d_type == Tok_CONST || la.d_type == Tok_IMPORT || la.d_type == Tok_PROC || la.d_type == Tok_VAR || la.d_type == Tok_BEGIN || la.d_type == Tok_END ) {
            DeclarationSequence();
        } else
            invalid("module");
    }
    if( FIRST_block(la.d_type) ) {
        IdentDef id;
        id.name= la;
        id.name.d_val = "$begin";
        id.visi = IdentDef::Private;
        Declaration* procDecl = addDecl(id, Declaration::Procedure);
        if( procDecl == 0 )
            return;
        mdl->openScope(procDecl);
        procDecl->body = block();
        mdl->closeScope();
    }
    expect(Tok_END, true, "module");
    expect(Tok_ident, false, "module");
    if( la.d_type == Tok_Dot ) {
        expect(Tok_Dot, false, "module");
    }
    mdl->closeScope();
}

void Parser2::ImportList() {
	expect(Tok_IMPORT, true, "ImportList");
	import();
	while( la.d_type == Tok_Comma || FIRST_import(la.d_type) ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "ImportList");
		}
		import();
	}
	if( la.d_type == Tok_Semi ) {
		expect(Tok_Semi, false, "ImportList");
	}
}

void Parser2::import() {
    Token localName;
    if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_ColonEq )  ) {
		expect(Tok_ident, false, "import");
        localName = cur;
        expect(Tok_ColonEq, false, "import");
	}
    TokenList tl;
    expect(Tok_ident, false, "import");
    tl << cur;
    while( la.d_type == Tok_Dot ) {
		expect(Tok_Dot, false, "import");
		expect(Tok_ident, false, "import");
        tl << cur;
    }
    if( localName.d_tokenType == 0 )
        localName = tl.last();

    Declaration* importDecl = addDecl(localName, 0, Declaration::Import);
    if( importDecl == 0 )
        return;

    QByteArrayList path;
    foreach( const Token& t, tl)
        path << t.d_val;
    importDecl->data = QVariant::fromValue(path);

    if( FIRST_MetaActuals(la.d_type) ) {
        expect(Tok_Lpar, false, "MetaActuals");
        importDecl->expr = ConstExpression();
        if( importDecl->expr == 0 )
            return;
        Expression* last = importDecl->expr;
        while( la.d_type == Tok_Comma || FIRST_ConstExpression(la.d_type) ) {
            if( la.d_type == Tok_Comma ) {
                expect(Tok_Comma, false, "MetaActuals");
            }
            Expression* e = ConstExpression();
            if( e == 0 )
                return;
            last->next = e;
            last = e;
        }
        expect(Tok_Rpar, false, "MetaActuals");
	}
}

MetaParamList Parser2::MetaParams() {
	expect(Tok_Lpar, false, "MetaParams");
    MetaParamList res;
    bool isType = true;
    res << MetaSection(isType);
    while( la.d_type == Tok_Semi || FIRST_MetaSection(la.d_type) ) {
		if( la.d_type == Tok_Semi ) {
			expect(Tok_Semi, false, "MetaParams");
		}
        res << MetaSection(isType);
	}
	expect(Tok_Rpar, false, "MetaParams");
    return res;
}

MetaParamList Parser2::MetaSection(bool& isType) {
	if( la.d_type == Tok_TYPE || la.d_type == Tok_CONST ) {
		if( la.d_type == Tok_TYPE ) {
			expect(Tok_TYPE, false, "MetaSection");
            isType = true;
        } else if( la.d_type == Tok_CONST ) {
			expect(Tok_CONST, false, "MetaSection");
            isType = false;
        } else
			invalid("MetaSection");
	}
    TokenList ids;
	expect(Tok_ident, false, "MetaSection");
    ids << cur;
	while( ( ( peek(1).d_type == Tok_Comma || peek(1).d_type == Tok_ident ) && peek(2).d_type == Tok_ident )  ) {
		if( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "MetaSection");
		}
		expect(Tok_ident, false, "MetaSection");
        ids << cur;
	}
    Type* t = 0;
	if( la.d_type == Tok_Colon ) {
		expect(Tok_Colon, false, "MetaSection");
        t = NamedType();
	}
    MetaParamList res;
    for( int i = 0; i < ids.size(); i++ )
    {
        Declaration* decl = addDecl(ids[i], 0, isType ? Declaration::TypeDecl : Declaration::ConstDecl);
        if( decl == 0 )
            continue;
        decl->meta = true;
        decl->type = t;
        if( t == 0 )
        {
            // each var get's a separate Type to avoid uncontrolled type equality
            decl->type = new Type();
            decl->type->form = Type::Generic;
            decl->ownstype = true;
        }
        res << decl;
    }
    return res;
}

Declaration*Parser2::addDecl(const Token& id, quint8 visi, quint8 mode)
{
    // NOTE: we don't check here whether names are unique; this is to be done in the validator
    Declaration* d = mdl->addDecl(id.d_val);
    if( d == 0 )
    {
        error( id, "a declaration with this name already exists");
        return 0;
    }
    d->mode = mode;
    d->visi = visi;
    d->pos.d_row = id.d_lineNr;
    d->pos.d_col = id.d_colNr;
    return d;
}

Declaration*Parser2::addDecl(const Parser2::IdentDef& id, quint8 mode)
{
    return addDecl(id.name, id.visi, mode);
}

void Parser2::error(const Token& t, const QString& msg)
{
    Q_ASSERT(!msg.isEmpty());
    errors << Error(msg,RowCol(t.d_lineNr, t.d_colNr), t.d_sourcePath);
}

void Parser2::error(const RowCol& pos, const QString& msg)
{
    Q_ASSERT(!msg.isEmpty());
    errors << Error(msg, pos, scanner->source());
}

Declaration*Parser2::addHelper(Type* t)
{
    Declaration* decl = mdl->addHelper();
    // we need these syntetic declarations because emitter doesn't support anonymous types
    decl->mode = Declaration::TypeDecl;
    decl->type = t;
    decl->ownstype = true;
    decl->outer = thisMod;
    t->decl = decl;
    t->anonymous = true;
    return decl;
}

void Parser2::clearTemps()
{
    foreach( Type* t, temporaries )
        delete t;
    temporaries.clear();
}
