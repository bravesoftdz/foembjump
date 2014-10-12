package com.bero.games.foembjump;
/*************************************
** 2-clause simplified BSD license ***
**************************************
**
** Copyright 2007-2011 Benjamin Rosseaux. All rights reserved.
**
** Redistribution and use in source and binary forms, with or without modification, are
** permitted provided that the following conditions are met:
**
**    1. Redistributions of source code must retain the above copyright notice, this list of
**       conditions and the following disclaimer.
**
**    2. Redistributions in binary form must reproduce the above copyright notice, this list
**       of conditions and the following disclaimer in the documentation and/or other materials
**       provided with the distribution.
**
** THIS SOFTWARE IS PROVIDED BY BENJAMIN ROSSEAUX ``AS IS'' AND ANY EXPRESS OR IMPLIED
** WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
** FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL BENJAMIN ROSSEAUX OR
** CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
** CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
** SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
** ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
** NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
** ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
** The views and conclusions contained in the software and documentation are those of the
** authors and should not be interpreted as representing official policies, either expressed
** or implied, of Benjamin Rosseaux.
*/



import java.nio.ByteBuffer;

final public class JumpGame{

	public long mInstance=0;

	static{
		System.loadLibrary("jumpgame");
	}

	public static native void nativeCreate(int sampleRate,int channels,int bits,int bufferSamples);

	public static native void nativeDestroy();

	public static native void nativeReinit(int width,int height);

	public static native void nativeLogicStep();
	
	public static native boolean nativeRenderStep();

	public static native void nativeFillBuffer(ByteBuffer buffer);

	public static native void nativeResume();
	
	public static native void nativeSuspend();

	public static native void nativeCleanUp();

	public static native void nativeAccelerometerChange(float x,float y,float z);

	public static native void nativeTouch(float x,float y);
	
	public static native int nativeGetStateSize();

	public static native void nativeSaveState(ByteBuffer buffer);
	
	public static native void nativeLoadState(ByteBuffer buffer,int size);

	public static native boolean nativeBack();
		
}