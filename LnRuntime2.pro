#/*
#* Copyright 2024 Rochus Keller <mailto:me@rochus-keller.ch>
#*
#* This file is part of the Luon IDE application.
#*
#* The following is the license that applies to this copy of the
#* application. For a license to use the application under conditions
#* other than those described here, please email to me@rochus-keller.ch.
#*
#* GNU General Public License Usage
#* This file may be used under the terms of the GNU General Public
#* License (GPL) versions 2.0 or 3.0 as published by the Free Software
#* Foundation and appearing in the file LICENSE.GPL included in
#* the packaging of this file. Please review the following information
#* to ensure GNU General Public Licensing requirements will be met:
#* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
#* http://www.gnu.org/copyleft/gpl.html.
#*/

QT       += core gui widgets
TARGET = luon
TEMPLATE = app

INCLUDEPATH += .. ../LuaJIT-2.1/src

DEFINES += _LJTOOLS_DONT_CREATE_TAIL_CALLS USE_JITCOMPOSER2

SOURCES += \
    ../LjTools/LuaJitBytecode2.cpp \
    ../LjTools/Engine2.cpp \
    ../LjTools/LuaJitComposer2.cpp \
    ../LjTools/LuaJitHelper.cpp \
    LnRuntime.cpp \
    LnLjRuntime.cpp \
    LnProject.cpp \
    LnLjbcGen.cpp \
    LnPAL3.cpp \
    LnBitBlt.c

HEADERS  += \
    ../LjTools/LuaJitBytecode2.h \
    ../LjTools/Engine2.h \
    ../LjTools/LuaJitComposer2.h \
    ../LjTools/LuaJitHelper.h \
    LnRuntime.h \
    LnLjRuntime.h \
    LnProject.h \
    LnLjbcGen.h \
    LnPAL3.h

win32 {
    LIBS += -L../LuaJIT-2.1/src -llua51
}
linux | macx {
    LIBS += $$absolute_path(../LuaJIT-2.1/src/libluajit.a, $$_PRO_FILE_PWD_)
    QMAKE_LFLAGS += -rdynamic -ldl
    #rdynamic is required so that the LjLibFfi functions are visible to LuaJIT FFI
    LIBS += -lSDL2
}

include( LnParser.pri )

CONFIG(debug, debug|release) {
        DEFINES += _DEBUG
    # DEFINES += LUA_USE_ASSERT
    # DEFINES +=  _INSERT_DGBTRACE
}

!win32 {
    QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable
}

RESOURCES += \
    LnIde.qrc

