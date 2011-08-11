// **********************************************************************
//
// Copyright (c) 2003-2007 ZeroC, Inc. All rights reserved.
//
// This copy of Ice is licensed to you under the terms described in the
// ICE_LICENSE file included in this distribution.
//
// **********************************************************************

#ifndef HELLO_I_H
#define HELLO_I_H

#include <Hello.h>

class HelloI : public Demo::Hello
{
public:

    virtual void sayHello(int delay, const Ice::Current&) const;
    virtual void shutdown(const Ice::Current&);
    virtual Demo::CSeq getvalues(int number, const Ice::Current&);
    virtual int output(int &oa1, int &oa2, const Ice::Current& c);
};

#endif
