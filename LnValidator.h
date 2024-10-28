#ifndef LNVALIDATOR_H
#define LNVALIDATOR_H

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

#include <Luon/LnAst.h>

namespace Ln
{
    class Importer {
    public:
        virtual Declaration* loadModule( const Import& imp ) = 0;
        virtual QByteArray moduleSuffix( const MetaActualList& imp ) = 0;
        virtual QByteArray modulePath( const QByteArrayList& imp ) = 0;
    };

    class Validator
    {
    public:
        Validator(AstModel* mdl, Importer* imp = 0, bool xref = false);
        ~Validator();

        bool validate(Declaration* module, const Import& import = Import());

        Xref takeXref();

        struct Error {
            QString msg;
            RowCol pos;
            QString path;
            Error( const QString& m, const RowCol& pos, const QString& p):msg(m),pos(pos),path(p){}
        };
        QList<Error> errors;
    protected:
        void error( const RowCol&, const QString& msg );
        void visitScope( Declaration* scope );
        void visitDecl( Declaration* );
        void visitImport(Declaration* import);
        void visitBody(Statement* body);
        void visitExpr(Expression*, Type* hint = 0);
        void visitType(Type*);
        void visitEnum(Type*);
        void resolve(Type* nameRef);
        void unaryOp(Expression*);
        void binaryOp(Expression*);
        void arithOp(Expression*);
        void relOp(Expression*);
        void assigOp(Statement*);
        Statement* ifOp(Statement*);
        Statement* caseStat(Statement*);
        Statement* forStat(Statement*);
        void loopStat(Statement*);
        void returnOp(Statement*);
        Qualident resolve(Expression* nameRef);
        typedef QPair<Declaration*,Declaration*> ResolvedQ;
        ResolvedQ find(const Qualident& q, const RowCol& pos, Declaration* import = 0);
        void selectOp(Expression*);
        void callOp(Expression*);
        void callOp(Statement*);
        void indexOp(Expression*);
        void inOp(Expression*);
        void isOp(Expression*);
        void constructor(Expression*, Type* hint);
        Type* deref(Type*) const;
        bool checkBuiltinArgs(quint8 builtin, const ExpList& args, Type** ret, const RowCol& pos);
        void markDecl(Declaration*);
        Symbol* markRef(Declaration*, const RowCol&);

        bool assigCompat(Type* lhs, Type* rhs) const;
        bool assigCompat(Type* lhs, Declaration* rhs) const;
        bool assigCompat(Type* lhs, const Expression* rhs) const;
        bool paramCompat(Declaration* lhs, const Expression* rhs) const;
        bool matchFormals(const QList<Declaration*>& a, const QList<Declaration*>& b) const;
        bool matchResultType(Type* lhs, Type* rhs) const;
        bool sameType(Type* lhs, Type* rhs) const;
        bool equalTypes(Type* lhs, Type* rhs) const;
    private:
        AstModel* mdl;
        Importer* imp;
        Declaration* module;
        ExpList unresolveds;
        QList<Declaration*> scopeStack;
        QList<Declaration*> boundProcs;
        QList<Statement*> loopStack;
        Symbol* first;
        Symbol* last;
        QHash<Declaration*,SymList> xref;
        QHash<Declaration*,DeclList> subs;
    };
}

#endif // LNVALIDATOR_H
