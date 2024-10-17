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
    class Validator
    {
    public:
        Validator(AstModel* mdl);

        bool validate(Declaration* module);

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
        void visitDecl( Declaration* scope );

    private:
        AstModel* mdl;
        Declaration* module;
    };
}

#endif // LNVALIDATOR_H
