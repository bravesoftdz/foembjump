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
import java.nio.ByteOrder;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.lang.Runnable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.opengles.GL10;

import com.bero.library.opengles.GLSurfaceView;

import android.app.Activity;
import android.content.Context;
import android.content.res.Configuration;
import android.graphics.PixelFormat;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.media.AudioFormat;
import android.media.AudioManager;
import android.media.AudioTrack;
import android.os.Bundle;
import android.util.Log;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.Surface;
import android.view.Window;
import android.view.WindowManager;
import android.os.Build;
import android.os.Process;
import android.view.Display;

@SuppressWarnings("unused")
public class MainActivtiy extends Activity implements SensorEventListener{
	private GL2View mGL2View;

	private static final String TAG="FoembJump";

	private static final int sampleRate=44100;
	private static final int channels=2;
	private static final int bits=16;

	private static final int bufferMilliseconds=10;

	private int mAudioConfiguration;
	private int mAudioEnconding;

	private AtomicBoolean mQuit=new AtomicBoolean(false);
	private AtomicBoolean mPaused=new AtomicBoolean(true);
	private Thread mAudioThread=null;
	private int mBufferSizeInBytes;
	private int mBufferSizeInSamples;
	private int mAudioSampleRate;
	private int mAudioBufferSizeInBytes;
	private int mAudioBufferSizeInSamples;
	private int mAudioSleepTime;

	private boolean mInitializated=false;
	private AtomicBoolean mReinit=new AtomicBoolean(true);

	private String mSysDevice=Build.DEVICE.toLowerCase().trim();
	private String mSysModel=Build.MODEL.toLowerCase().trim();
	private String mSysProduct=Build.PRODUCT.toLowerCase().trim();

	private AtomicBoolean mNativeInitializated=new AtomicBoolean(false);

	private SensorManager mSensorManager;
	private Sensor mAccelerometer;

	private Object mSyncObj=new Object();

	private final static int mAtomicAccelXFactor=65536;
	private final static float mAtomicAccelXFactorInv=1.0f/mAtomicAccelXFactor;
	private AtomicLong mAtomicAccelX=new AtomicLong(0);

	private int stateSize=0;
	private ByteBuffer stateByteBuffer=null;
	private boolean hasState=false;

	private Display mDisplay=null;
	private int mDisplayRotation=0;

	private Runnable mRunnableInitUI=new Runnable(){

		@Override
		public void run(){
			mGL2View=new GL2View(getApplication());
			setContentView(mGL2View);

			mSensorManager=(SensorManager)getSystemService(Context.SENSOR_SERVICE);
			if(mSensorManager.getSensorList(Sensor.TYPE_ACCELEROMETER).size()>0){
				mAccelerometer=mSensorManager.getSensorList(Sensor.TYPE_ACCELEROMETER).get(0);
				mSensorManager.registerListener(MainActivtiy.this,mAccelerometer,SensorManager.SENSOR_DELAY_GAME);
			}

			mAudioThread=new Thread(mAudioThreadRunnable);
			mAudioThread.start();

			mNativeInitializated.set(true);

			mReinit.set(true);

			mLogicThread=new Thread(mLogicThreadRunnable);
			mLogicThread.start();

			doResume();

		}

	};

	private Runnable mRunnableInit=new Runnable(){

		@Override
		public void run(){
			mAudioSampleRate=sampleRate;

			switch(channels){
				case 1:{
					mAudioConfiguration=AudioFormat.CHANNEL_CONFIGURATION_MONO;
					break;
				}
				case 2:{
					mAudioConfiguration=AudioFormat.CHANNEL_CONFIGURATION_STEREO;
					break;
				}
				default:{
					finish();
					break;
				}
			}

			switch(bits){
				case 8:{
					mAudioEnconding=AudioFormat.ENCODING_PCM_8BIT;
					break;
				}
				case 16:{
					mAudioEnconding=AudioFormat.ENCODING_PCM_16BIT;
					break;
				}
				default:{
					finish();
					break;
				}
			}

			mBufferSizeInSamples=((sampleRate*bufferMilliseconds)+500)/1000;
			mBufferSizeInBytes=(mBufferSizeInSamples*(channels*bits))>>>3;

			mAudioBufferSizeInBytes=AudioTrack.getMinBufferSize(mAudioSampleRate,mAudioConfiguration,mAudioEnconding);
			mAudioBufferSizeInSamples=(mAudioBufferSizeInBytes<<3)/(channels*bits);

			mAudioSleepTime=Math.min(Math.max(bufferMilliseconds>>2,10),100);

			synchronized(mSyncObj){
				JumpGame.nativeCreate(sampleRate,channels,bits,mBufferSizeInSamples);
				if(hasState&&(stateByteBuffer!=null)){
					stateByteBuffer.position(0);
					JumpGame.nativeLoadState(stateByteBuffer,stateSize);
				}
			}

			runOnUiThread(mRunnableInitUI);
		}

	};

	private Thread mInitThread=new Thread(mRunnableInit);

	private static Method mDisplay_getRotation,mDisplay_getOrientation;

	@Override
	protected void onCreate(Bundle savedInstanceState){
		super.onCreate(savedInstanceState);

		mDisplay=((WindowManager)getSystemService(Context.WINDOW_SERVICE)).getDefaultDisplay();
		if(mDisplay!=null){
			try{
				mDisplay_getRotation=mDisplay.getClass().getMethod("getRotation");
				try{
					mDisplayRotation=((Integer)mDisplay_getRotation.invoke(mDisplay,(Object[])null)).intValue();
				}catch(IllegalArgumentException iae){
			 		mDisplayRotation=Surface.ROTATION_0;
				}catch(IllegalAccessException iae){
					mDisplayRotation=Surface.ROTATION_0;
				}catch(InvocationTargetException ite){
					mDisplayRotation=Surface.ROTATION_0;
				}
			}catch(NoSuchMethodException nsme){
				try{
					mDisplay_getOrientation=mDisplay.getClass().getMethod("getOrientation");
					try{
						switch(((Integer)mDisplay_getOrientation.invoke(mDisplay,(Object[])null)).intValue()){
							case Configuration.ORIENTATION_PORTRAIT:{
								mDisplayRotation=Surface.ROTATION_0;
								break;
							}
							case Configuration.ORIENTATION_LANDSCAPE:{
								mDisplayRotation=Surface.ROTATION_90;
								break;
							}
							default:{
								if(mDisplay.getWidth()<mDisplay.getHeight()){
									mDisplayRotation=Surface.ROTATION_0;
								}else{
									mDisplayRotation=Surface.ROTATION_90;
								}
								break;
							}
						}
					}catch(IllegalArgumentException iae){
						mDisplayRotation=Surface.ROTATION_0;
					}catch(IllegalAccessException iae){
						mDisplayRotation=Surface.ROTATION_0;
					}catch(InvocationTargetException ite){
						mDisplayRotation=Surface.ROTATION_0;
					}
				}catch(NoSuchMethodException nsme2){
					mDisplayRotation=Surface.ROTATION_0;
				}
			}
		}else{
			mDisplayRotation=Surface.ROTATION_0;
		}

//      Log.i("FoembJump","mDisplayRotation: "+mDisplayRotation);

		stateSize=JumpGame.nativeGetStateSize();
		stateByteBuffer=ByteBuffer.allocateDirect(stateSize).order(ByteOrder.nativeOrder());

		hasState=false;
		if(savedInstanceState!=null){
			if(savedInstanceState.containsKey("gamestate")){
				byte[] stateByteArray=savedInstanceState.getByteArray("gamestate");
				if(stateByteArray!=null){
					if(stateByteArray.length==stateSize){
						stateByteBuffer.position(0);
						stateByteBuffer.put(stateByteArray);
						stateByteBuffer.position(0);
						stateByteArray=null;
						hasState=true;
					}
				}
			}
		}

		this.requestWindowFeature(Window.FEATURE_NO_TITLE);
		this.getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,WindowManager.LayoutParams.FLAG_FULLSCREEN);

		setContentView(R.layout.main);

		mInitializated=false;

		mQuit.set(false);
		mPaused.set(true);

		mNativeInitializated.set(false);

	}

	@Override
	protected void onDestroy(){
		if(mNativeInitializated.get()){
			try{
				if(mSensorManager.getSensorList(Sensor.TYPE_ACCELEROMETER).size()>0){
					mSensorManager.unregisterListener(this);
				}
			}catch(Exception e){
			}
			mQuit.set(true);
			mPaused.set(false);
			try{
				int tries=100;
				while((tries-->0)&&mAudioThread.isAlive()){
					Thread.sleep(100);
				}
				if(mAudioThread.isAlive()){
					// Still alive? KILL IT!
					mAudioThread.stop();
				}
			}catch(InterruptedException e){
				e.printStackTrace();
			}
			try{
				int tries=100;
				while((tries-->0)&&mLogicThread.isAlive()){
					Thread.sleep(100);
				}
				if(mLogicThread.isAlive()){
					// Still alive? KILL IT!
					mLogicThread.stop();
				}
			}catch(InterruptedException e){
				e.printStackTrace();
			}
			synchronized(mSyncObj){
				JumpGame.nativeDestroy();
			}
		}
		killMySelf();
		super.onDestroy();
	}

	@Override
	protected void onSaveInstanceState(Bundle outState){
		Log.d(TAG,"Enter onSaveInstanceState();");
		super.onSaveInstanceState(outState);
		if(mNativeInitializated.get()&&mInitializated){
			ByteBuffer localStateByteBuffer=ByteBuffer.allocateDirect(stateSize).order(ByteOrder.nativeOrder());
			localStateByteBuffer.position(0);
			synchronized(mSyncObj){
				JumpGame.nativeSaveState(localStateByteBuffer);
			}
			localStateByteBuffer.position(0);
			byte[] localStateByteArray=new byte[stateSize];
			localStateByteBuffer.get(localStateByteArray,0,stateSize);
			outState.putByteArray("gamestate",localStateByteArray);
			localStateByteArray=null;
			localStateByteBuffer.clear();
			localStateByteBuffer=null;
		}
		Log.d(TAG,"Leave onSaveInstanceState();");
	}

	@Override
	protected void onPause(){
		super.onPause();
		if(mNativeInitializated.get()){
			try{
				if(mSensorManager.getSensorList(Sensor.TYPE_ACCELEROMETER).size()>0){
					mSensorManager.unregisterListener(this);
				}
			}catch(Exception e){
			}
			mQuit.set(false);
			mPaused.set(true);
			if(mGL2View!=null){
				mGL2View.onPause();
			}
			synchronized(mSyncObj){
				JumpGame.nativeSuspend();
			}
			mReinit.set(true);
			mAccelX=0.0f;
			mOldAccelX=1.0f;
		}
		setContentView(R.layout.blank);
		mGL2View=null;
	}

	@Override
	protected void onStart(){
		super.onStart();
		if(!mNativeInitializated.get()){
			mInitThread.start();
		}
	}

	public void doResume(){
		mQuit.set(false);
		mPaused.set(false);
		mReinit.set(true);
		try{
			if(mSensorManager.getSensorList(Sensor.TYPE_ACCELEROMETER).size()>0){
				mSensorManager.registerListener(this,mAccelerometer,SensorManager.SENSOR_DELAY_GAME);
			}
		}catch(Exception e){
		}
		synchronized(mSyncObj){
			JumpGame.nativeResume();
		}
		mReinit.set(true);
		mAccelX=0.0f;
		mOldAccelX=1.0f;

	}

	private Runnable mOnResumeRunnable=new Runnable(){
		@Override
		public void run(){
			if(mNativeInitializated.get()){
				mGL2View=new GL2View(getApplication());
				setContentView(mGL2View);
				mGL2View.onResume();
				doResume();
			}
		}
	};

	@Override
	protected void onResume(){
		super.onResume();
		runOnUiThread(mOnResumeRunnable);
	}

	private Runnable mLogicThreadRunnable=new Runnable(){
		public void run(){
			Log.d(TAG,"Starting logic thread");
			try{
				while(!mQuit.get()){
					if(mNativeInitializated.get()&&!(mPaused.get()||mReinit.get())){
						synchronized(mSyncObj){
							JumpGame.nativeAccelerometerChange(mAtomicAccelX.get()*mAtomicAccelXFactorInv,0,0);
							JumpGame.nativeLogicStep();
						}
					}
					Thread.sleep(10);
				}
			}catch(Exception e){
				Log.e(TAG,"Error in logic thread.",e);
			}
			Log.d(TAG,"Stopping logic thread");
		}
	};

	private Thread mLogicThread;

	private Runnable mAudioThreadRunnable=new Runnable(){
		public void run(){
			Log.d(TAG,"Starting audio track thread");
			final int bufferSizeInBytes=mBufferSizeInBytes;
			final int sleepTime=mAudioSleepTime;
			byte[] byteArray=new byte[bufferSizeInBytes];
			ByteBuffer byteBuffer=ByteBuffer.allocateDirect(byteArray.length).order(ByteOrder.nativeOrder());
			AudioTrack track=new AudioTrack(AudioManager.STREAM_MUSIC,mAudioSampleRate,mAudioConfiguration,mAudioEnconding,mAudioBufferSizeInBytes,AudioTrack.MODE_STREAM);
			int offset=0,len=0,bytes=0;
			int bufferLength=byteArray.length;
			boolean wasPaused=true;
			Process.setThreadPriority(Process.THREAD_PRIORITY_AUDIO);
			try{
				while(!mQuit.get()){
					if(mNativeInitializated.get()&&!mPaused.get()){
						if(wasPaused||(offset>=byteBuffer.capacity())){
							if(wasPaused){
								wasPaused=false;
								track.play();
							}
							byteBuffer.position(0);
							JumpGame.nativeFillBuffer(byteBuffer);
							byteBuffer.position(0);
							byteBuffer.get(byteArray);
							offset=0;
						}
						len=bufferLength-offset;
						bytes=track.write(byteArray,offset,len);
						if(bytes<0){
							break;
						}else if(bytes!=len){
							Thread.sleep(sleepTime);
						}
						offset+=bytes;
					}else{
						if(!wasPaused){
							track.stop();
							wasPaused=true;
						}
						Thread.sleep(100);
					}
				}
			}catch(Exception e){
				Log.e(TAG,"Error in audio track thread.",e);
			}
			if(!wasPaused){
				track.stop();
			}
			track.release();
			Log.d(TAG,"Stopping audio track thread");
		}
	};

	private class GL2View extends GLSurfaceView implements GLSurfaceView.Renderer{

		public int mWidth=480,mHeight=800;

		public String[] mGLRenderer=null;

		public GL2View(Context context){
			super(context);

			setKeepScreenOn(true);

			setEGLContextClientVersion(2);

			try{
				mGLRenderer=getGLRenderer(context);
				if(mGLRenderer[1].contains("PowerVR SGX 530")||mGLRenderer[1].contains("PowerVR SGX 535")){
					getHolder().setFormat(PixelFormat.RGB_565);
					setEGLConfigChooser(5,6,5,0,0,0);
				}else{
					getHolder().setFormat(PixelFormat.RGBA_8888);
					setEGLConfigChooser(8,8,8,8,0,0);
				}
			}catch(Exception e){
				e.printStackTrace();
				if((mSysModel.equals("milestone")||mSysProduct.equals("umts_sholes")||mSysDevice.equals("umts_sholes"))){
					getHolder().setFormat(PixelFormat.RGB_565);
					setEGLConfigChooser(5,6,5,0,0,0);
				}else{
					getHolder().setFormat(PixelFormat.RGBA_8888);
					setEGLConfigChooser(8,8,8,8,0,0);
				}
			}

			setRenderer(this);
			setRenderMode(RENDERMODE_CONTINUOUSLY);
		}

		public boolean onDrawFrame(GL10 gl){
			if(mReinit.get()){
				synchronized(mSyncObj){
					JumpGame.nativeReinit(mWidth,mHeight);
				}
				mReinit.set(false);
			}
			return JumpGame.nativeRenderStep();
		}

		public void onSurfaceChanged(GL10 gl,int width,int height){
			mWidth=width;
			mHeight=height;
			synchronized(mSyncObj){
				JumpGame.nativeReinit(width,height);
			}
			mReinit.set(false);
			mInitializated=true;
		}

		public void onSurfaceCreated(GL10 gl,EGLConfig config){
			synchronized(mSyncObj){
				JumpGame.nativeReinit(mWidth,mHeight);
			}
			mReinit.set(false);
		}

		@Override
		public void onCleanUp(GL10 gl){
			synchronized(mSyncObj){
				JumpGame.nativeCleanUp();
			}
			mInitializated=false;
		}

	}

	@Override
	public boolean onTouchEvent(final MotionEvent event){
		if(mInitializated){
			if(event.getAction()==MotionEvent.ACTION_DOWN){
				synchronized(mSyncObj){
					JumpGame.nativeTouch(event.getX(),event.getY());
				}
			}
		}
		return true;
	}

	@Override
	public void onAccuracyChanged(Sensor sensor,int accuracy){
		// TODO Auto-generated method stub

	}

	private final static float xmax=10.0f;

	private final static float mAccelLowPassFactor=0.9f;
	private final static float mAccelHighPassFactor=1e-8f;
	private float mAccelX=0.0f;
	private float mOldAccelX=1.0f;

	@Override
	public void onSensorChanged(SensorEvent event){
		if(mNativeInitializated.get()&&!mPaused.get()){
			switch(event.sensor.getType()){
				case Sensor.TYPE_ACCELEROMETER:{
					float t=0.0f;
					switch(mDisplayRotation){
						case Surface.ROTATION_0:{
							t=event.values[0];
							break;
						}
						case Surface.ROTATION_90:{
							t=-event.values[1];
							break;
						}
						case Surface.ROTATION_180:{
							t=-event.values[0];
							break;
						}
						case Surface.ROTATION_270:{
							t=event.values[1];
							break;
						}
						default:{
							t=0.0f;
							break;
						}
					}
					mAccelX=(mAccelX*(1.0f-mAccelLowPassFactor))+(t*mAccelLowPassFactor);
					float x=mAccelX;
					if(Math.abs(x)>=1.0f){
						x=-((x-((x<0.0f)?-1.0f:1.0f))*2.0f);
						if(x<-xmax){
							x=-xmax;
						}else if(x>xmax){
							x=xmax;
						}
					}else{
						x=0.0f;
					}
					if(x!=mOldAccelX){
						mOldAccelX=x;
						mAtomicAccelX.set((long)(x*mAtomicAccelXFactor));
						// / synchronized(mSyncObj){
						// JumpGame.nativeAccelerometerChange(x,0,0);
						// }
					}
					break;
				}
				case Sensor.TYPE_ORIENTATION:{
					break;
				}
				case Sensor.TYPE_MAGNETIC_FIELD:{
					break;
				}
			}
		}
	}

	@Override
	public boolean onKeyDown(int keyCode,KeyEvent event){
		if(mNativeInitializated.get()){
			if(keyCode==KeyEvent.KEYCODE_MENU){
			}else if(keyCode==KeyEvent.KEYCODE_BACK){
				if(!JumpGame.nativeBack()){
					finish();
				}
				return true;

			}
		}
		return super.onKeyDown(keyCode,event);
	}

	private void killMySelf(){
		android.os.Process.killProcess(android.os.Process.myPid());
		System.exit(0);
	}

}
