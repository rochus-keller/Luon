#ifndef LN_ROWCOL_H
#define LN_ROWCOL_H

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

#include <QString>
#include <QPair>

namespace Ln
{
    struct RowCol
    {
        enum { ROW_BIT_LEN = 19, COL_BIT_LEN = 32 - ROW_BIT_LEN - 1 };
        uint d_row : ROW_BIT_LEN; // supports 524k lines
        uint d_col : COL_BIT_LEN; // supports 4k chars per line
        uint unused : 1; // the sign is used to recognize the packed representation

        RowCol():d_row(0),d_col(0),unused(0) {}
        RowCol( quint32 row, quint32 col );
        bool setRowCol( quint32 row, quint32 col );
        bool isValid() const { return d_row > 0 && d_col > 0; } // valid lines and cols start with 1; 0 is invalid
        bool operator==( const RowCol& rhs ) const { return d_row == rhs.d_row && d_col == rhs.d_col; }

        quint32 packed() const { return ( d_row << COL_BIT_LEN ) | d_col; }
        static bool isPacked( quint32 rowCol ) { return rowCol; }
        static quint32 unpackCol(quint32 rowCol ) { return rowCol & ( ( 1 << COL_BIT_LEN ) -1 ); }
        static quint32 unpackRow(quint32 rowCol ) { return ( ( rowCol ) >> COL_BIT_LEN ); }
    };

    typedef QPair<RowCol,RowCol> Range;
    typedef QList<Range> Ranges;
    struct FilePos
    {
        RowCol d_pos;
        QString d_filePath;
        FilePos(RowCol pos = RowCol(), const QString& path = QString()):d_pos(pos),d_filePath(path){}
    };
}

#endif // LN_ROWCOL_H
