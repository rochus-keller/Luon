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
TARGET = LuonIDE
TEMPLATE = app

INCLUDEPATH += .. ../LuaJIT/src

DEFINES += _LJTOOLS_DONT_CREATE_TAIL_CALLS HAVE_SDL

SOURCES += LnIde.cpp \
    LnHighlighter.cpp \
    ../GuiTools/CodeEditor.cpp \
    ../LjTools/LuaJitBytecode.cpp \
    ../LjTools/Engine2.cpp \
    ../LjTools/Terminal2.cpp \
    ../LjTools/ExpressionParser.cpp \
    ../LjTools/LuaJitComposer.cpp \
    ../LjTools/BcViewer2.cpp \
    ../GuiTools/DocSelector.cpp \
    ../GuiTools/DocTabWidget.cpp \
    ../LjTools/BcViewer.cpp \
    ../LjTools/LjDisasm.cpp \ 
    LnLjRuntime.cpp \
    LnProject.cpp \
    LnLjbcGen.cpp \
    LnPAL.c \
    LnPAL2.c \
    LnPAL3.cpp \
    LnBitBlt.c

HEADERS  += LnIde.h \
    LnHighlighter.h \
    ../GuiTools/CodeEditor.h \
    ../LjTools/LuaJitBytecode.h \
    ../LjTools/Engine2.h \
    ../LjTools/Terminal2.h \
    ../LjTools/ExpressionParser.h \
    ../LjTools/LuaJitComposer.h \
    ../LjTools/BcViewer2.h \
    ../GuiTools/DocSelector.h \
    ../GuiTools/DocTabWidget.h \
    ../LjTools/BcViewer.h \
    ../LjTools/LjDisasm.h \ 
    LnLjRuntime.h \
    LnProject.h \
    LnLjbcGen.h \
    LnPAL3.h

win32 {
    LIBS += -L../LuaJIT/src -llua51
}
linux {
    include( ../LuaJIT/src/LuaJit.pri ){
        LIBS += -ldl
    } else {
        LIBS += -lluajit
    }
    QMAKE_LFLAGS += -rdynamic -ldl
    #rdynamic is required so that the LjLibFfi functions are visible to LuaJIT FFI
    LIBS += -lSDL2
}
macx {
    include( ../LuaJIT/src/LuaJit.pri )
    QMAKE_LFLAGS += -rdynamic -ldl -pagezero_size 10000 -image_base 100000000
}

include( LnParser.pri )
include( ../GuiTools/Menu.pri )

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

