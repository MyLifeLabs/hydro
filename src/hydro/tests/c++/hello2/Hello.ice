// **********************************************************************
//
// Copyright (c) 2003-2007 ZeroC, Inc. All rights reserved.
//
// This copy of Ice is licensed to you under the terms described in the
// ICE_LICENSE file included in this distribution.
//
// **********************************************************************

#ifndef HELLO_ICE
#define HELLO_ICE

module Demo
{

class C {
    string s;
};

sequence<C> CSeq;

interface Hello
{
    ["cpp:const"] idempotent void sayHello(int delay);
    CSeq getvalues(int number);
    int output(out int oa1, out int oa2);
    idempotent void shutdown();
};

};

#endif
