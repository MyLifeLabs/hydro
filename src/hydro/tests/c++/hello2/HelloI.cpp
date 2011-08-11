// **********************************************************************
//
// Copyright (c) 2003-2007 ZeroC, Inc. All rights reserved.
//
// This copy of Ice is licensed to you under the terms described in the
// ICE_LICENSE file included in this distribution.
//
// **********************************************************************

#include <HelloI.h>
#include <Ice/Ice.h>
#include <IceUtil/IceUtil.h>

using namespace std;

void
HelloI::sayHello(int delay, const Ice::Current&) const
{
    if(delay != 0)
    {
        IceUtil::ThreadControl::sleep(IceUtil::Time::milliSeconds(delay));
    }
    cout << "Hello World!" << endl;
}

void
HelloI::shutdown(const Ice::Current& c)
{
    cout << "Shutting down..." << endl;
    c.adapter->getCommunicator()->shutdown();
}

Demo::CSeq
HelloI::getvalues(int number, const Ice::Current& c)
{
    int k;
    Demo::CSeq vec(number);
    cout << "getvalues" << endl;
    for (k=0; k<number; k++) {
	vec[k] = new Demo::C;
	vec[k]->s = "a string";
    }
    return vec;

}

int HelloI::output(int &oa1, int &oa2, const Ice::Current& c)
{
    oa1=1;
    oa2=2;
    return 3;
}
