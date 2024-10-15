/*
* Copyright 2024 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Luon language project.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
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
#include "LnRowCol.h"
#include <QtDebug>
using namespace Ln;

RowCol::RowCol(quint32 row, quint32 col)
{
    if( !setRowCol(row,col) )
        qWarning() << "invalid row or column number";
}

bool RowCol::setRowCol(quint32 row, quint32 col)
{
    static const quint32 maxRow = ( 1 << ROW_BIT_LEN ) - 1;
    static const quint32 maxCol = ( 1 << COL_BIT_LEN ) - 1;
    int err = 0;
    if( row > maxRow )
    {
        d_row = maxRow;
        err++;
    }else if( row == 0 )
    {
        d_row = 1;
        err++;
    }else
        d_row = row;
    if( col > maxCol )
    {
        d_col = maxCol;
        err++;
    }else if( col == 0 )
    {
        d_col = 1;
        err++;
    }else
        d_col = col;
    return err == 0;
}
