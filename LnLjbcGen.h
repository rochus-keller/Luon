#ifndef LNLJBCGEN_H
#define LNLJBCGEN_H

/*
* Copyright 2024 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Luon parser/compiler library.
*
* The following is the license that applies to this copy of the
* library. For a license to use the library under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/

#include <Luon/LnAst.h>

namespace Ln
{
    class LjbcGen
    {
    public:
        LjbcGen();
        ~LjbcGen();

        bool translate(Declaration* module, QIODevice* out, bool strip = false);
        static bool allocateModuleSlots(Declaration* module);

        struct Error {
            QString msg;
            RowCol pos;
            QString path;
            Error( const QString& m, const RowCol& pos, const QString& p):msg(m),pos(pos),path(p){}
        };

        const QList<Error>& getErrors() const;
    private:
        class Imp;
        Imp* imp;
    };
}

#endif // LNLJBCGEN_H
