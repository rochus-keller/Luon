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

Validator::Validator(AstModel* mdl):mdl(mdl)
{
    Q_ASSERT(mdl);
}

bool Validator::validate(Declaration* module)
{
    Q_ASSERT(module);
    this->module = module;
    visitScope(module);
    return !errors.isEmpty();
}

void Validator::error(const RowCol& pos, const QString& msg)
{
    ModuleData md = module->data.value<ModuleData>();
    errors << Error(msg, pos, md.source);
}

void Validator::visitScope(Declaration* scope)
{
    Declaration* cur = scope->link;
    while( cur )
    {
        if( !cur->validated )
            visitDecl(cur);
        cur = cur->getNext();
    }
}

void Validator::visitDecl(Declaration* d)
{
    switch( d->mode )
    {
    case Declaration::TypeDecl:
        break;
    case Declaration::ConstDecl:
        break;
    case Declaration::Import:
        break;
    case Declaration::VarDecl:
        break;
    case Declaration::LocalDecl:
        break;
    case Declaration::Procedure:
        break;
    case Declaration::ParamDecl:
        break;
    }
}

