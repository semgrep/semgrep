/*******************************************************************************
 * Copyright (c) 2007, 2017 IBM Corporation and others.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Karsten Thoms <karsten.thoms@itemis.de> - Bug 522349
 *******************************************************************************/
package org.eclipse.swt.internal.cocoa;

import org.eclipse.swt.internal.*;

public class OS extends C {
	static {
		Library.loadLibrary("swt-pi"); //$NON-NLS-1$
	}

	public static final int VERSION;

	public static int VERSION (int major, int minor, int bugfix) {
		return (major << 16) + (minor << 8) + bugfix;
	}

	/*
	 *  Magic number explanation, from Cocoa's TextSizingExample:
	 *
	 *  "The first is called LargeNumberForText [1.0e7] and it was not arbitrarily chosen.
	 *  The actual value was chosen to be around the largest floating point value possible that can preserve at least pixel precision. [...]
	 *  It is not wise to use bigger dimensions for text system objects because, even if you ever fill all that space,
	 *  by the time you get to the far reaches, the letters won't have the precision necessary to look and act correctly.
	 *  [...] Because the Cocoa text system respects this limit in various ways, a second constant, NotQuiteAsLargeNumberForText, is used for the
	 *  field-like text views created by the FieldAspect class. This is simply half of LargeNumberForText; at sizes as large as LargeNumberForText,
	 *  the text system stops aligning text, for various reasons."
	 */
	public static final double MAX_TEXT_CONTAINER_SIZE = 0.5e7f;

	public static final int noErr = 0;
	public static final int kSystemIconsCreator = ('m' << 24) + ('a' << 16) + ('c' << 8) + 's';
	public static final int kAlertCautionIcon = ('c'<<24) + ('a'<<16) + ('u'<<8) + 't';
	public static final int kAlertNoteIcon = ('n'<<24) + ('o'<<16) + ('t'<<8) + 'e';
	public static final int kAlertStopIcon = ('s'<<24) + ('t'<<16) + ('o'<<8) + 'p';
	public static final int shiftKey = 1 << 9;
	public static final int kThemeMetricFocusRectOutset = 7;
	public static final int kHIThemeOrientationNormal = 0;
	public static final int kUIModeNormal = 0;
	public static final int kUIModeContentHidden = 2;
	public static final int kUIModeAllHidden = 3;
	public static final int kLSUnknownType = 0;
	public static final int kLSUnknownCreator = 0;
	public static final int kLSRolesAll = 0xFFFFFFFF;
	public static final int kAXUnderlineStyleNone = 0x0;
	public static final int kAXUnderlineStyleSingle = 0x1;
	public static final int kAXUnderlineStyleThick = 0x2;
	public static final int kAXUnderlineStyleDouble = 0x9;
	public static final int kPMDestinationPrinter = 1;
	public static final int kPMDuplexNone = 0x0001;
	public static final int kPMDuplexNoTumble = 0x0002;
	public static final int kPMDuplexTumble = 0x0003;

	public static final long sel_identity = Selector.sel_identity.value;
	public static final long sel_sendSearchSelection = Selector.sel_sendSearchSelection.value;
	public static final long sel_sendCancelSelection = Selector.sel_sendCancelSelection.value;
	public static final long sel_sendSelection = Selector.sel_sendSelection.value;
	public static final long sel_sendSelection_ = Selector.sel_sendSelection_.value;
	public static final long sel_sendDoubleSelection = Selector.sel_sendDoubleSelection.value;
	public static final long sel_sendVerticalSelection = Selector.sel_sendVerticalSelection.value;
	public static final long sel_sendHorizontalSelection = Selector.sel_sendHorizontalSelection.value;
	public static final long sel_timerProc_ = Selector.sel_timerProc_.value;
	public static final long sel_callJava = Selector.sel_callJava.value;
	public static final long sel_callRunBeforeUnloadConfirmPanelWithMessage = Selector.sel_callRunBeforeUnloadConfirmPanelWithMessage.value;
	public static final long sel_createPanelDidEnd = Selector.sel_createPanelDidEnd.value;
	public static final long sel_systemColorSettingsChanged_ = Selector.sel_systemColorSettingsChanged_.value;
	public static final long sel_screenParametersChanged_ = Selector.sel_screenParametersChanged_.value;
	public static final long sel_panelDidEnd_returnCode_contextInfo_ = Selector.sel_panelDidEnd_returnCode_contextInfo_.value;
	public static final long sel_updateOpenGLContext_ = Selector.sel_updateOpenGLContext_.value;

	public static final long sel_overwriteExistingFileCheck = Selector.sel_overwriteExistingFileCheck.value;

	public static final long sel__drawThemeProgressArea_ = Selector.sel__drawThemeProgressArea_.value;

	public static final long sel__setDashboardBehavior = Selector.sel__setDashboardBehavior.value;

	public static final long class_WebPanelAuthenticationHandler = OS.objc_getClass("WebPanelAuthenticationHandler");
	public static final long sel_sharedHandler = Selector.sel_sharedHandler.value;
	public static final long sel_startAuthentication = Selector.sel_startAuthentication.value;
	public static final long sel_setAllowsAnyHTTPSCertificate = Selector.sel_setAllowsAnyHTTPSCertificate.value;

	public static final long sel_accessibleHandle = Selector.sel_accessibleHandle.value;
	public static final long sel_getImageView = Selector.sel_getImageView.value;

	public static final long sel_clearDeferFlushing = Selector.sel_clearDeferFlushing.value;

	public static final long sel_setShouldExpandItem_ = Selector.sel_setShouldExpandItem_.value;
	public static final long sel_setShouldScrollClipView_ = Selector.sel_setShouldScrollClipView_.value;

	public static final long sel_setQuota = Selector.sel_setQuota.value;
	public static final long sel_webView_frame_exceededDatabaseQuotaForSecurityOrigin_database_ = Selector.sel_webView_frame_exceededDatabaseQuotaForSecurityOrigin_database.value;

	public static final long sel_beginSheetModalForWindow_completionHandler_ = Selector.sel_beginSheetModalForWindow_completionHandler_.value;

	/** custom selector for SWT.OpenUrl event, also used in the launcher **/
	public static final long sel_application_openUrls_ = Selector.sel_application_openUrls_.value;

	/** non-API selector for NSCursor **/
	public static final long sel_busyButClickableCursor = Selector.sel_busyButClickableCursor.value;

	/* These are not generated in order to avoid creating static methods on all classes */
	public static final long sel_isSelectorExcludedFromWebScript_ = Selector.sel_isSelectorExcludedFromWebScript_.value;
	public static final long sel_webScriptNameForSelector_ = Selector.sel_webScriptNameForSelector_.value;

	public static final long sel_setColor_forAttribute_ = Selector.sel_setColor_forAttribute_.value;

	public static final long sel_javaRunLoopMode = Selector.sel_javaRunLoopMode.value;

	/* These are not generated in order to avoid attempting to create a java method called "null" */
	public static final long class_NSNull = objc_getClass("NSNull");
	public static final long sel_null = Selector.sel_null.value;

	/* NSTextAttachmentCell */
	/** @method callback_types=NSPoint;id;SEL;,callback_flags=struct;none;none; */
	public static final native long CALLBACK_cellBaselineOffset(long func);
	/** @method callback_types=NSSize;id;SEL;,callback_flags=struct;none;none; */
	public static final native long CALLBACK_NSTextAttachmentCell_cellSize(long func);
	/** @method callback_types=id;id;SEL;,callback_flags=struct;none;none; */
	public static final native long CALLBACK_NSTextAttachmentCell_attachment(long func);
	public static final long sel_cellBaselineOffset = Selector.sel_cellBaselineOffset.value;

	/** 10.12 selector */
	public static final long sel_setAllowsAutomaticWindowTabbing_ = Selector.sel_setAllowsAutomaticWindowTabbing.value;

	/* AWT application delegate. Remove these when JavaRuntimeSupport.framework has bridgesupport generated for it. */
	public static final long class_JRSAppKitAWT = objc_getClass("JRSAppKitAWT");
	public static final long sel_awtAppDelegate = Selector.sel_awtAppDelegate.value;

	public static final long class_NSToolbarView = objc_getClass("NSToolbarView");

	/*
	 * Wrapper function which will call NSSavePanel.beginSheetModalForWindow. This
	 * implementation allows passing of objective-C block from Java to C code, and
	 * receives a callback from the block to a Java function. Here, handler is a
	 * the function pointer of the function that will be called by the objective-C
	 * block.
	 */
	/** @method flags=no_gen*/
	public static native void beginSheetModalForWindow(long id, long sel, long window, long handler);
	public static void beginSheetModalForWindow(NSPanel id, NSWindow window, long handler) {
		OS.beginSheetModalForWindow(id.id, OS.sel_beginSheetModalForWindow_completionHandler_, window != null ? window.id : 0, handler);
	}

	/*
	 * Custom message that will be sent when setTheme is called for example from Platform UI code.
	 */
	public static final long sel_appAppearanceChanged = OS.sel_registerName("appAppearanceChanged");
	public static void setTheme(boolean isDarkTheme) {
		OS.objc_msgSend(NSApplication.sharedApplication().id, sel_appAppearanceChanged, isDarkTheme ? 1 : 0);
	}
	public static boolean isAppDarkAppearance() {
		if (OS.VERSION >= OS.VERSION(10, 14, 0)) {
			NSAppearance currentAppearance = NSAppearance.currentAppearance();
			if (currentAppearance != null) {
				return "NSAppearanceNameDarkAqua".equals(currentAppearance.name().getString());
			}
		}
		return false;
	}
	public static boolean isSystemDarkAppearance() {
		if (OS.VERSION >= OS.VERSION(10, 14, 0)) {
			NSString osxMode = NSUserDefaults.standardUserDefaults ().stringForKey (NSString.stringWith ("AppleInterfaceStyle"));
			return osxMode != null && "Dark".equals (osxMode.getString ());
		}
		return false;
	}

/** JNI natives */

/** @method flags=jni */
public static final native long NewGlobalRef(Object object);
/**
 * @method flags=jni
 * @param globalRef cast=(jobject)
 */
public static final native void DeleteGlobalRef(long globalRef);
/** @method flags=no_gen */
public static final native Object JNIGetObject(long globalRef);

/** Carbon calls */

/** @param psn cast=(ProcessSerialNumber *) */
public static final native int GetCurrentProcess(int[] psn);
public static final native int CPSSetProcessName(int[] psn, long name);
/** @method flags=dynamic */
/** @method flags=dynamic
	@param inCreator cast=(OSType)
	@param inType cast=(OSType)
	@param inExtension cast=(CFStringRef)
	@param inMIMEType cast=(CFStringRef)
	@param inUsageFlags cast=(IconServicesUsageFlags)
	@param outIconRef cast=(IconRef *) */
public static final native int GetIconRefFromTypeInfo(int inCreator, int inType, long inExtension, long inMIMEType, int inUsageFlags, long outIconRef[]);
/** @method flags=dynamic */
public static final native long TISCopyCurrentKeyboardInputSource();
/** @method flags=dynamic
	@param inputSource cast=(TISInputSourceRef)
	@param propertyKey cast=(CFStringRef) */
public static final native long TISGetInputSourceProperty (long inputSource, long propertyKey);
/** @method flags=no_gen */
public static final native long kTISPropertyUnicodeKeyLayoutData();
/**
 * @method flags=dynamic
 * @param inMode cast=(UInt32)
 * @param inOptions cast=(UInt32)
 */
public static final native int SetSystemUIMode(int inMode, int inOptions);
/**
 * @method flags=dynamic
 * @param outMode cast=(UInt32*)
 * @param outOptions cast=(UInt32*)
 */
public static final native int GetSystemUIMode(int[] outMode, int[] outOptions);
/**
 * @method flags=dynamic
 * @param keyLayoutPtr cast=(const UCKeyboardLayout *)
 * @param virtualKeyCode cast=(UInt16)
 * @param keyAction cast=(UInt16)
 * @param modifierKeyState cast=(UInt32)
 * @param keyboardType cast=(UInt32)
 * @param keyTranslateOptions cast=(OptionBits)
 * @param deadKeyState cast=(UInt32 *)
 * @param maxStringLength cast=(UniCharCount)
 * @param actualStringLength cast=(UniCharCount *)
 * @param unicodeString cast=(UniChar *)
 */
public static final native int UCKeyTranslate (long keyLayoutPtr, short virtualKeyCode, short keyAction, int modifierKeyState, int keyboardType, int keyTranslateOptions, int[] deadKeyState, int maxStringLength, long[] actualStringLength, char[] unicodeString);
/**
 * @param inUTI1 cast=(CFStringRef)
 * @param inUTI2 cast=(CFStringRef)
 */
public static final native boolean UTTypeEqual(long inUTI1, long inUTI2);

/**
 * @method flags=dynamic
 * @param metric cast=(SInt32 *)
*/
public static final native void GetThemeMetric(int themeConstant, int[] metric);
/**
 * @method flags=dynamic
 * @param inContext cast=(CGContextRef)
*/
public static final native int HIThemeDrawFocusRect(CGRect inRect, boolean inHasFocus, long inContext, int inOrientation);

public static final int kUCKeyActionDown = 0;
public static final int kUCKeyActionUp = 1;
/** @method flags=dynamic */
public static final native byte LMGetKbdType();

/** @method flags=dynamic */
public static final native long AcquireRootMenu ();
/** @method flags=dynamic */
public static final native int CancelMenuTracking (long inRootMenu, boolean inImmediate, int inDismissalReason);
/**
 * @param inType cast=(OSType)
 * @param inCreator cast=(OSType)
 * @param inExtension cast=(CFStringRef)
 * @param inRoleMask cast=(LSRolesMask)
 * @param outAppRef cast=(FSRef *)
 * @param outAppURL cast=(CFURLRef *)
 */
public static final native long LSGetApplicationForInfo(int inType, int inCreator,long inExtension, int inRoleMask, byte[] outAppRef, int[] outAppURL);
/** @method flags=dynamic
 * @param pmSessionInfo cast=(PMPrintSession)
 * @param outPMPrinter cast=(PMPrinter *)
 */
public static final native long PMSessionGetCurrentPrinter(long pmSessionInfo, long[] outPMPrinter);
/** @method flags=dynamic
 * @param pmSessionInfo cast=(PMPrintSession)
 * @param pmPrintSettings cast=(PMPrintSettings)
 */
public static final native long PMSessionGetDestinationType(long pmSessionInfo, long pmPrintSettings, short[] outDestinationType);
/** @method flags=dynamic
 * @param printSettings cast=(PMPrintSettings)
 * @param outDuplexSetting cast=(PMDuplexMode *)
 */
public static final native long PMGetDuplex(long printSettings, int[] outDuplexSetting);
/** @method flags=dynamic
 * @param printSettings cast=(PMPrintSettings)
 * @param duplexSetting cast=(PMDuplexMode)
 */
public static final native long PMSetDuplex(long printSettings, int duplexSetting);
/** @method flags=dynamic
 * @param pmPrinter cast=(PMPrinter)
 * @param outNumResolutions cast=(UInt32 *)
 */
public static final native long PMPrinterGetPrinterResolutionCount(long pmPrinter, int[] outNumResolutions);
/** @method flags=dynamic
 * @param pmPrinter cast=(PMPrinter)
 * @param pmPrintSettings cast=(PMPrintSettings)
 * @param outResolution cast=(PMResolution *)
 */
public static final native long PMPrinterGetOutputResolution(long pmPrinter, long pmPrintSettings, PMResolution outResolution);
/** @method flags=dynamic
 * @param pmPrinter cast=(PMPrinter)
 * @param outResolution cast=(PMResolution *)
 */
public static final native long PMPrinterGetIndexedPrinterResolution(long pmPrinter, int index, PMResolution outResolution);

/** C calls */

public static final native int getpid();

public static final native void call(long proc, long id, long sel);

/** QuickDraw calls */

/** @method flags=dynamic */
public static final native long NewRgn();
/** @method flags=dynamic */
public static final native void RectRgn(long rgnHandle, short[] rect);
/** @method flags=dynamic */
public static final native void OpenRgn();
/** @method flags=dynamic */
public static final native void OffsetRgn(long rgnHandle, short dh, short dv);
/** @method flags=dynamic */
public static final native void MoveTo(short h, short v);
/** @method flags=dynamic */
public static final native void LineTo(short h, short v);
/** @method flags=dynamic */
public static final native void UnionRgn(long srcRgnA, long srcRgnB, long dstRgn);
/** @method flags=dynamic */
public static final native void CloseRgn(long dstRgn);
/** @method flags=dynamic */
public static final native void DisposeRgn(long rgnHandle);
/**
 * @method flags=dynamic
 * @param pt flags=struct,cast=(Point *)
 */
public static final native boolean PtInRgn(short[] pt, long rgnHandle);
/** @method flags=dynamic */
public static final native void GetRegionBounds(long rgnHandle, short[] bounds);
/** @method flags=dynamic */
public static final native void SectRgn(long srcRgnA, long srcRgnB, long dstRgn);
/** @method flags=dynamic */
public static final native boolean EmptyRgn(long rgnHandle);
/** @method flags=dynamic */
public static final native void DiffRgn(long srcRgnA, long srcRgnB, long dstRgn);
/** @method flags=dynamic */
public static final native boolean RectInRgn(short[] rect, long rgnHandle);
/** @method flags=dynamic */
public static final native int QDRegionToRects(long rgn, int dir, long proc, long userData);
/** @method flags=dynamic */
public static final native void CopyRgn(long srcRgnHandle, long dstRgnHandle);
/** @method flags=dynamic */
public static final native void SetRect(short[] r, short left, short top, short right, short bottom);
public static final int kQDParseRegionFromTop = (1 << 0);
public static final int kQDParseRegionFromLeft = (1 << 2);
public static final int kQDParseRegionFromTopLeft = kQDParseRegionFromTop | kQDParseRegionFromLeft;
public static final int kQDRegionToRectsMsgParse = 2;

/** JavaScriptCore calls */

/**
 * @param ctx cast=(JSContextRef)
 * @param script cast=(JSStringRef)
 * @param thisObject cast=(JSObjectRef)
 * @param sourceURL cast=(JSStringRef)
 * @param exception cast=(JSValueRef *)
 */
public static final native long JSEvaluateScript (long ctx, long script, long thisObject, long sourceURL, int startingLineNumber, long[] exception);

/**
 * @param string cast=(const char *)
 */
public static final native long JSStringCreateWithUTF8CString (byte[] string);

/**
 * @param string cast=(JSStringRef)
 */
public static final native void JSStringRelease (long string);


/** Certificate Security */

/**
 * @param certType cast=(CSSM_CERT_TYPE)
 * @param policyOID cast=(CSSM_OID *)
 * @param value cast=(CSSM_DATA *)
 * @param policySearch cast=(SecPolicySearchRef *)
 */
public static final native int SecPolicySearchCreate(long certType, long policyOID, long value, long [] policySearch);

/**
 * @param searchRef cast=(SecPolicySearchRef)
 * @param policyRef cast=(SecPolicyRef *)
 */
public static final native int SecPolicySearchCopyNext(long searchRef, long [] policyRef);

/**
 * @param certificates cast=(CFArrayRef)
 * @param policies cast=(CFTypeRef)
 * @param trustRef cast=(SecTrustRef *)
 */
public static final native int SecTrustCreateWithCertificates(long certificates, long policies, long [] trustRef);

public static final int CSSM_CERT_X_509v3 = 0x3;

/** Custom callbacks */

/** @method flags=no_gen */
public static final native long isFlipped_CALLBACK();

/** Custom structure return */

/** @method flags=no_gen */
public static final native void NSIntersectionRect (NSRect result, NSRect aRect, NSRect bRect);
/**
 * @method flags=no_gen
 * @param display cast=(CGDirectDisplayID)
 */
public static final native void CGDisplayBounds(int display, CGRect rect);

/** @method flags=const */
public static final native long kUTTypeFileURL();
public static final NSString kUTTypeFileURL = new NSString(kUTTypeFileURL());
/** @method flags=const */
public static final native long kUTTypeURL();
public static final NSString kUTTypeURL = new NSString(kUTTypeURL());

/** Objective-C runtime */

/**
 * @param cls cast=(Class)
 * @param name cast=(const char *),flags=critical
 * @param types cast=(const char *),flags=critical
 */
public static final native boolean class_addIvar(long cls, byte[] name, long size, byte alignment, byte[] types);
/**
 * @param cls cast=(Class)
 * @param name cast=(SEL)
 * @param imp cast=(IMP)
 */
public static final native boolean class_addMethod(long cls, long name, long imp, String types);
/**
 * @param cls cast=(Class)
 * @param protocol cast=(Protocol *)
 */
public static final native boolean class_addProtocol(long cls, long protocol);
/**
 * @param aClass cast=(Class)
 * @param aSelector cast=(SEL)
 */
public static final native long class_getClassMethod(long aClass, long aSelector);
/**
 * @param cls cast=(Class)
 * @param name cast=(SEL)
 */
public static final native long class_getMethodImplementation(long cls, long name);
/**
 * @param cls cast=(Class)
 * @param name cast=(SEL)
 */
public static final native long class_getInstanceMethod(long cls, long name);
/** @param cls cast=(Class) */
public static final native long class_getSuperclass(long cls);
/**
 * @param method cast=(Method)
 * @param imp cast=(IMP)
 */
public static final native long method_setImplementation(long method, long imp);
/**
 * @param cls cast=(Class)
 * @param extraBytes cast=(size_t)
 */
public static final native long class_createInstance(long cls, long extraBytes);
/** @method flags=no_gen */
public static final native String class_getName(long cls);
/** @method flags=dynamic */
public static final native void instrumentObjcMessageSends(boolean val);
/** @param superclass cast=(Class) */
public static final native long objc_allocateClassPair(long superclass, String name, long extraBytes);
/** @param klass cast=(Class) */
public static final native void objc_disposeClassPair(long klass);
public static final native long objc_getClass(String className);
public static final native long objc_getMetaClass(String name);
public static final native long objc_getProtocol(String name);
public static final native long objc_lookUpClass(String className);
/** @param cls cast=(Class) */
public static final native void objc_registerClassPair(long cls);
/** @param obj cast=(id) */
public static final native long object_getClassName(long obj);
/** @param obj cast=(id) */
public static final native long object_getClass(long obj);

/**
 * @param obj cast=(id)
 * @param name cast=(const char*),flags=critical
 * @param outValue cast=(void **),flags=critical
 */
public static final native long object_getInstanceVariable(long obj, byte[] name, long [] outValue);
/**
 * @param obj cast=(id)
 * @param name cast=(const char*),flags=critical
 * @param value cast=(void *),flags=critical
 */
public static final native long object_setInstanceVariable(long obj, byte[] name, long value);
/**
 * @param obj cast=(id)
 * @param clazz cast=(Class)
 */
public static final native long object_setClass(long obj, long clazz);
public static final native long sel_registerName(String selectorName);
public static final native int objc_super_sizeof();

/** This section is auto generated */

/** Custom callbacks */
/** @method callback_types=id;id;SEL;NSPoint;,callback_flags=none;none;none;struct; */
public static final native long CALLBACK_accessibilityHitTest_(long func);
/** @method callback_types=NSAttributedString*;id;SEL;NSRange;,callback_flags=none;none;none;struct; */
public static final native long CALLBACK_attributedSubstringFromRange_(long func);
/** @method callback_types=void;id;SEL;NSRect;NSBitmapImageRep*;,callback_flags=none;none;none;struct;none; */
public static final native long CALLBACK_cacheDisplayInRect_toBitmapImageRep_(long func);
/** @method callback_types=BOOL;id;SEL;NSIndexSet*;NSPoint;,callback_flags=none;none;none;none;struct; */
public static final native long CALLBACK_canDragRowsWithIndexes_atPoint_(long func);
/** @method callback_types=NSSize;id;SEL;,callback_flags=struct;none;none; */
public static final native long CALLBACK_cellSize(long func);
/** @method callback_types=NSSize;id;SEL;NSRect;,callback_flags=struct;none;none;struct; */
public static final native long CALLBACK_cellSizeForBounds_(long func);
/** @method callback_types=NSUInteger;id;SEL;NSPoint;,callback_flags=none;none;none;struct; */
public static final native long CALLBACK_characterIndexForPoint_(long func);
/** @method callback_types=NSInteger;id;SEL;NSPoint;,callback_flags=none;none;none;struct; */
public static final native long CALLBACK_columnAtPoint_(long func);
/** @method callback_types=void;id;SEL;NSImage*;NSPoint;NSDragOperation;,callback_flags=none;none;none;none;struct;none; */
public static final native long CALLBACK_draggedImage_endedAt_operation_(long func);
/** @method callback_types=void;id;SEL;NSRect;,callback_flags=none;none;none;struct; */
public static final native long CALLBACK_drawBackgroundInClipRect_(long func);
/** @method callback_types=void;id;SEL;NSRect;NSView*;,callback_flags=none;none;none;struct;none; */
public static final native long CALLBACK_drawBezelWithFrame_inView_(long func);
/** @method callback_types=void;id;SEL;NSImage*;NSRect;NSView*;,callback_flags=none;none;none;none;struct;none; */
public static final native long CALLBACK_drawImage_withFrame_inView_(long func);
/** @method callback_types=void;id;SEL;NSRect;NSView*;,callback_flags=none;none;none;struct;none; */
public static final native long CALLBACK_drawInteriorWithFrame_inView_(long func);
/** @method callback_types=void;id;SEL;BOOL;NSRect;,callback_flags=none;none;none;none;struct; */
public static final native long CALLBACK_drawLabel_inRect_(long func);
/** @method callback_types=void;id;SEL;NSRect;,callback_flags=none;none;none;struct; */
public static final native long CALLBACK_drawRect_(long func);
/** @method callback_types=NSRect;id;SEL;NSAttributedString*;NSRect;NSView*;,callback_flags=struct;none;none;none;struct;none; */
public static final native long CALLBACK_drawTitle_withFrame_inView_(long func);
/** @method callback_types=void;id;SEL;NSRect;,callback_flags=none;none;none;struct; */
public static final native long CALLBACK_drawViewBackgroundInRect_(long func);
/** @method callback_types=void;id;SEL;NSRect;NSView*;,callback_flags=none;none;none;struct;none; */
public static final native long CALLBACK_drawWithExpansionFrame_inView_(long func);
/** @method callback_types=NSRect;id;SEL;NSRect;NSView*;,callback_flags=struct;none;none;struct;none; */
public static final native long CALLBACK_expansionFrameWithFrame_inView_(long func);
/** @method callback_types=NSRect;id;SEL;NSRange;,callback_flags=struct;none;none;struct; */
public static final native long CALLBACK_firstRectForCharacterRange_(long func);
/** @method callback_types=NSRect;id;SEL;NSRect;NSView*;,callback_flags=struct;none;none;struct;none; */
public static final native long CALLBACK_focusRingMaskBoundsForFrame_inView_(long func);
/** @method callback_types=NSRect;id;SEL;NSInteger;,callback_flags=struct;none;none;none; */
public static final native long CALLBACK_headerRectOfColumn_(long func);
/** @method callback_types=void;id;SEL;NSRect;,callback_flags=none;none;none;struct; */
public static final native long CALLBACK_highlightSelectionInClipRect_(long func);
/** @method callback_types=NSView*;id;SEL;NSPoint;,callback_flags=none;none;none;struct; */
public static final native long CALLBACK_hitTest_(long func);
/** @method callback_types=NSCellHitResult;id;SEL;NSEvent*;NSRect;NSView*;,callback_flags=none;none;none;none;struct;none; */
public static final native long CALLBACK_hitTestForEvent_inRect_ofView_(long func);
/** @method callback_types=NSRect;id;SEL;NSRect;,callback_flags=struct;none;none;struct; */
public static final native long CALLBACK_imageRectForBounds_(long func);
/** @method callback_types=NSRange;id;SEL;,callback_flags=struct;none;none; */
public static final native long CALLBACK_markedRange(long func);
/** @method callback_types=void;id;SEL;NSClipView*;NSPoint;,callback_flags=none;none;none;none;struct; */
public static final native long CALLBACK_scrollClipView_toPoint_(long func);
/** @method callback_types=NSRange;id;SEL;,callback_flags=struct;none;none; */
public static final native long CALLBACK_selectedRange(long func);
/** @method callback_types=void;id;SEL;NSPoint;,callback_flags=none;none;none;struct; */
public static final native long CALLBACK_setFrameOrigin_(long func);
/** @method callback_types=void;id;SEL;NSSize;,callback_flags=none;none;none;struct; */
public static final native long CALLBACK_setFrameSize_(long func);
/** @method callback_types=void;id;SEL;id;NSRange;,callback_flags=none;none;none;none;struct; */
public static final native long CALLBACK_setMarkedText_selectedRange_(long func);
/** @method callback_types=void;id;SEL;NSRect;,callback_flags=none;none;none;struct; */
public static final native long CALLBACK_setNeedsDisplayInRect_(long func);
/** @method callback_types=BOOL;id;SEL;NSRange;NSString*;,callback_flags=none;none;none;struct;none; */
public static final native long CALLBACK_shouldChangeTextInRange_replacementString_(long func);
/** @method callback_types=NSSize;id;SEL;BOOL;,callback_flags=struct;none;none;none; */
public static final native long CALLBACK_sizeOfLabel_(long func);
/** @method callback_types=NSRange;id;SEL;NSTextView*;NSRange;NSRange;,callback_flags=struct;none;none;none;struct;struct; */
public static final native long CALLBACK_textView_willChangeSelectionFromCharacterRange_toCharacterRange_(long func);
/** @method callback_types=NSRect;id;SEL;NSRect;,callback_flags=struct;none;none;struct; */
public static final native long CALLBACK_titleRectForBounds_(long func);
/** @method callback_types=NSString*;id;SEL;NSView*;NSToolTipTag;NSPoint;void*;,callback_flags=none;none;none;none;none;struct;none; */
public static final native long CALLBACK_view_stringForToolTip_point_userData_(long func);
/** @method callback_types=void;id;SEL;WebView*;NSRect;,callback_flags=none;none;none;none;struct; */
public static final native long CALLBACK_webView_setFrame_(long func);

/** Classes */
public static final long class_DOMDocument = objc_getClass("DOMDocument");
public static final long class_DOMEvent = objc_getClass("DOMEvent");
public static final long class_DOMKeyboardEvent = objc_getClass("DOMKeyboardEvent");
public static final long class_DOMMouseEvent = objc_getClass("DOMMouseEvent");
public static final long class_DOMUIEvent = objc_getClass("DOMUIEvent");
public static final long class_DOMWheelEvent = objc_getClass("DOMWheelEvent");
public static final long class_NSActionCell = objc_getClass("NSActionCell");
public static final long class_NSAffineTransform = objc_getClass("NSAffineTransform");
public static final long class_NSAlert = objc_getClass("NSAlert");
public static final long class_NSAppearance = objc_getClass("NSAppearance");
public static final long class_NSAppleEventDescriptor = objc_getClass("NSAppleEventDescriptor");
public static final long class_NSApplication = objc_getClass("NSApplication");
public static final long class_NSArray = objc_getClass("NSArray");
public static final long class_NSAttributedString = objc_getClass("NSAttributedString");
public static final long class_NSAutoreleasePool = objc_getClass("NSAutoreleasePool");
public static final long class_NSBezierPath = objc_getClass("NSBezierPath");
public static final long class_NSBitmapImageRep = objc_getClass("NSBitmapImageRep");
public static final long class_NSBox = objc_getClass("NSBox");
public static final long class_NSBundle = objc_getClass("NSBundle");
public static final long class_NSButton = objc_getClass("NSButton");
public static final long class_NSButtonCell = objc_getClass("NSButtonCell");
public static final long class_NSCalendarDate = objc_getClass("NSCalendarDate");
public static final long class_NSCell = objc_getClass("NSCell");
public static final long class_NSClipView = objc_getClass("NSClipView");
public static final long class_NSCoder = objc_getClass("NSCoder");
public static final long class_NSColor = objc_getClass("NSColor");
public static final long class_NSColorList = objc_getClass("NSColorList");
public static final long class_NSColorPanel = objc_getClass("NSColorPanel");
public static final long class_NSColorSpace = objc_getClass("NSColorSpace");
public static final long class_NSComboBox = objc_getClass("NSComboBox");
public static final long class_NSComboBoxCell = objc_getClass("NSComboBoxCell");
public static final long class_NSControl = objc_getClass("NSControl");
public static final long class_NSCursor = objc_getClass("NSCursor");
public static final long class_NSData = objc_getClass("NSData");
public static final long class_NSDate = objc_getClass("NSDate");
public static final long class_NSDatePicker = objc_getClass("NSDatePicker");
public static final long class_NSDictionary = objc_getClass("NSDictionary");
public static final long class_NSDirectoryEnumerator = objc_getClass("NSDirectoryEnumerator");
public static final long class_NSDockTile = objc_getClass("NSDockTile");
public static final long class_NSEnumerator = objc_getClass("NSEnumerator");
public static final long class_NSError = objc_getClass("NSError");
public static final long class_NSEvent = objc_getClass("NSEvent");
public static final long class_NSFileManager = objc_getClass("NSFileManager");
public static final long class_NSFileWrapper = objc_getClass("NSFileWrapper");
public static final long class_NSFont = objc_getClass("NSFont");
public static final long class_NSFontManager = objc_getClass("NSFontManager");
public static final long class_NSFontPanel = objc_getClass("NSFontPanel");
public static final long class_NSFormatter = objc_getClass("NSFormatter");
public static final long class_NSGradient = objc_getClass("NSGradient");
public static final long class_NSGraphicsContext = objc_getClass("NSGraphicsContext");
public static final long class_NSHTTPCookie = objc_getClass("NSHTTPCookie");
public static final long class_NSHTTPCookieStorage = objc_getClass("NSHTTPCookieStorage");
public static final long class_NSImage = objc_getClass("NSImage");
public static final long class_NSImageRep = objc_getClass("NSImageRep");
public static final long class_NSImageView = objc_getClass("NSImageView");
public static final long class_NSIndexSet = objc_getClass("NSIndexSet");
public static final long class_NSInputManager = objc_getClass("NSInputManager");
public static final long class_NSKeyedArchiver = objc_getClass("NSKeyedArchiver");
public static final long class_NSKeyedUnarchiver = objc_getClass("NSKeyedUnarchiver");
public static final long class_NSLayoutManager = objc_getClass("NSLayoutManager");
public static final long class_NSLocale = objc_getClass("NSLocale");
public static final long class_NSMenu = objc_getClass("NSMenu");
public static final long class_NSMenuItem = objc_getClass("NSMenuItem");
public static final long class_NSMutableArray = objc_getClass("NSMutableArray");
public static final long class_NSMutableAttributedString = objc_getClass("NSMutableAttributedString");
public static final long class_NSMutableDictionary = objc_getClass("NSMutableDictionary");
public static final long class_NSMutableIndexSet = objc_getClass("NSMutableIndexSet");
public static final long class_NSMutableParagraphStyle = objc_getClass("NSMutableParagraphStyle");
public static final long class_NSMutableSet = objc_getClass("NSMutableSet");
public static final long class_NSMutableString = objc_getClass("NSMutableString");
public static final long class_NSMutableURLRequest = objc_getClass("NSMutableURLRequest");
public static final long class_NSNotification = objc_getClass("NSNotification");
public static final long class_NSNotificationCenter = objc_getClass("NSNotificationCenter");
public static final long class_NSNumber = objc_getClass("NSNumber");
public static final long class_NSNumberFormatter = objc_getClass("NSNumberFormatter");
public static final long class_NSObject = objc_getClass("NSObject");
public static final long class_NSOpenGLContext = objc_getClass("NSOpenGLContext");
public static final long class_NSOpenGLPixelFormat = objc_getClass("NSOpenGLPixelFormat");
public static final long class_NSOpenPanel = objc_getClass("NSOpenPanel");
public static final long class_NSOutlineView = objc_getClass("NSOutlineView");
public static final long class_NSPanel = objc_getClass("NSPanel");
public static final long class_NSParagraphStyle = objc_getClass("NSParagraphStyle");
public static final long class_NSPasteboard = objc_getClass("NSPasteboard");
public static final long class_NSPopUpButton = objc_getClass("NSPopUpButton");
public static final long class_NSPrintInfo = objc_getClass("NSPrintInfo");
public static final long class_NSPrintOperation = objc_getClass("NSPrintOperation");
public static final long class_NSPrintPanel = objc_getClass("NSPrintPanel");
public static final long class_NSPrinter = objc_getClass("NSPrinter");
public static final long class_NSProcessInfo = objc_getClass("NSProcessInfo");
public static final long class_NSProgressIndicator = objc_getClass("NSProgressIndicator");
public static final long class_NSResponder = objc_getClass("NSResponder");
public static final long class_NSRunLoop = objc_getClass("NSRunLoop");
public static final long class_NSRunningApplication = objc_getClass("NSRunningApplication");
public static final long class_NSSavePanel = objc_getClass("NSSavePanel");
public static final long class_NSScreen = objc_getClass("NSScreen");
public static final long class_NSScrollView = objc_getClass("NSScrollView");
public static final long class_NSScroller = objc_getClass("NSScroller");
public static final long class_NSSearchField = objc_getClass("NSSearchField");
public static final long class_NSSearchFieldCell = objc_getClass("NSSearchFieldCell");
public static final long class_NSSecureTextField = objc_getClass("NSSecureTextField");
public static final long class_NSSet = objc_getClass("NSSet");
public static final long class_NSSlider = objc_getClass("NSSlider");
public static final long class_NSStatusBar = objc_getClass("NSStatusBar");
public static final long class_NSStatusItem = objc_getClass("NSStatusItem");
public static final long class_NSStepper = objc_getClass("NSStepper");
public static final long class_NSString = objc_getClass("NSString");
public static final long class_NSTabView = objc_getClass("NSTabView");
public static final long class_NSTabViewItem = objc_getClass("NSTabViewItem");
public static final long class_NSTableColumn = objc_getClass("NSTableColumn");
public static final long class_NSTableHeaderCell = objc_getClass("NSTableHeaderCell");
public static final long class_NSTableHeaderView = objc_getClass("NSTableHeaderView");
public static final long class_NSTableView = objc_getClass("NSTableView");
public static final long class_NSText = objc_getClass("NSText");
public static final long class_NSTextAttachment = objc_getClass("NSTextAttachment");
public static final long class_NSTextContainer = objc_getClass("NSTextContainer");
public static final long class_NSTextField = objc_getClass("NSTextField");
public static final long class_NSTextFieldCell = objc_getClass("NSTextFieldCell");
public static final long class_NSTextStorage = objc_getClass("NSTextStorage");
public static final long class_NSTextTab = objc_getClass("NSTextTab");
public static final long class_NSTextView = objc_getClass("NSTextView");
public static final long class_NSThread = objc_getClass("NSThread");
public static final long class_NSTimeZone = objc_getClass("NSTimeZone");
public static final long class_NSTimer = objc_getClass("NSTimer");
public static final long class_NSToolbar = objc_getClass("NSToolbar");
public static final long class_NSToolbarItem = objc_getClass("NSToolbarItem");
public static final long class_NSTouch = objc_getClass("NSTouch");
public static final long class_NSTrackingArea = objc_getClass("NSTrackingArea");
public static final long class_NSTypesetter = objc_getClass("NSTypesetter");
public static final long class_NSURL = objc_getClass("NSURL");
public static final long class_NSURLAuthenticationChallenge = objc_getClass("NSURLAuthenticationChallenge");
public static final long class_NSURLCredential = objc_getClass("NSURLCredential");
public static final long class_NSURLDownload = objc_getClass("NSURLDownload");
public static final long class_NSURLProtectionSpace = objc_getClass("NSURLProtectionSpace");
public static final long class_NSURLRequest = objc_getClass("NSURLRequest");
public static final long class_NSUndoManager = objc_getClass("NSUndoManager");
public static final long class_NSUserDefaults = objc_getClass("NSUserDefaults");
public static final long class_NSValue = objc_getClass("NSValue");
public static final long class_NSView = objc_getClass("NSView");
public static final long class_NSWindow = objc_getClass("NSWindow");
public static final long class_NSWorkspace = objc_getClass("NSWorkspace");
public static final long class_SFCertificatePanel = objc_getClass("SFCertificatePanel");
public static final long class_SFCertificateTrustPanel = objc_getClass("SFCertificateTrustPanel");
public static final long class_WebDataSource = objc_getClass("WebDataSource");
public static final long class_WebFrame = objc_getClass("WebFrame");
public static final long class_WebFrameView = objc_getClass("WebFrameView");
public static final long class_WebPreferences = objc_getClass("WebPreferences");
public static final long class_WebScriptObject = objc_getClass("WebScriptObject");
public static final long class_WebUndefined = objc_getClass("WebUndefined");
public static final long class_WebView = objc_getClass("WebView");

/** Protocols */
public static final long protocol_NSAccessibility = objc_getProtocol("NSAccessibility");
public static final long protocol_NSAccessibilityAdditions = objc_getProtocol("NSAccessibilityAdditions");
public static final long protocol_NSAppearanceCustomization = objc_getProtocol("NSAppearanceCustomization");
public static final long protocol_NSApplicationDelegate = objc_getProtocol("NSApplicationDelegate");
public static final long protocol_NSColorPanelResponderMethod = objc_getProtocol("NSColorPanelResponderMethod");
public static final long protocol_NSComboBoxDelegate = objc_getProtocol("NSComboBoxDelegate");
public static final long protocol_NSDraggingDestination = objc_getProtocol("NSDraggingDestination");
public static final long protocol_NSDraggingSourceDeprecated = objc_getProtocol("NSDraggingSourceDeprecated");
public static final long protocol_NSFontManagerResponderMethod = objc_getProtocol("NSFontManagerResponderMethod");
public static final long protocol_NSFontPanelValidationAdditions = objc_getProtocol("NSFontPanelValidationAdditions");
public static final long protocol_NSMenuDelegate = objc_getProtocol("NSMenuDelegate");
public static final long protocol_NSMenuValidation = objc_getProtocol("NSMenuValidation");
public static final long protocol_NSOpenSavePanelDelegate = objc_getProtocol("NSOpenSavePanelDelegate");
public static final long protocol_NSOutlineViewDataSource = objc_getProtocol("NSOutlineViewDataSource");
public static final long protocol_NSOutlineViewDelegate = objc_getProtocol("NSOutlineViewDelegate");
public static final long protocol_NSPasteboardOwner = objc_getProtocol("NSPasteboardOwner");
public static final long protocol_NSTabViewDelegate = objc_getProtocol("NSTabViewDelegate");
public static final long protocol_NSTableViewDataSource = objc_getProtocol("NSTableViewDataSource");
public static final long protocol_NSTableViewDelegate = objc_getProtocol("NSTableViewDelegate");
public static final long protocol_NSTextAttachmentCell = objc_getProtocol("NSTextAttachmentCell");
public static final long protocol_NSTextDelegate = objc_getProtocol("NSTextDelegate");
public static final long protocol_NSTextInput = objc_getProtocol("NSTextInput");
public static final long protocol_NSTextInputClient = objc_getProtocol("NSTextInputClient");
public static final long protocol_NSTextViewDelegate = objc_getProtocol("NSTextViewDelegate");
public static final long protocol_NSToolTipOwner = objc_getProtocol("NSToolTipOwner");
public static final long protocol_NSToolbarDelegate = objc_getProtocol("NSToolbarDelegate");
public static final long protocol_NSURLDownloadDelegate = objc_getProtocol("NSURLDownloadDelegate");
public static final long protocol_NSWindowDelegate = objc_getProtocol("NSWindowDelegate");
public static final long protocol_WebDocumentRepresentation = objc_getProtocol("WebDocumentRepresentation");
public static final long protocol_WebFrameLoadDelegate = objc_getProtocol("WebFrameLoadDelegate");
public static final long protocol_WebOpenPanelResultListener = objc_getProtocol("WebOpenPanelResultListener");
public static final long protocol_WebPolicyDecisionListener = objc_getProtocol("WebPolicyDecisionListener");
public static final long protocol_WebPolicyDelegate = objc_getProtocol("WebPolicyDelegate");
public static final long protocol_WebResourceLoadDelegate = objc_getProtocol("WebResourceLoadDelegate");
public static final long protocol_WebUIDelegate = objc_getProtocol("WebUIDelegate");

/** Selectors */
private static java.util.Map<Long,Selector> SELECTORS;
public static void registerSelector (Long value, Selector selector) {
	if (SELECTORS == null) {
		SELECTORS = new java.util.HashMap<>();
	}
	SELECTORS.put(value, selector);
}
public static Selector getSelector (long value) {
	return SELECTORS.get(value);
}
public static final long sel_CGEvent = Selector.sel_CGEvent.value;
public static final long sel_DOMDocument = Selector.sel_DOMDocument.value;
public static final long sel_IBeamCursor = Selector.sel_IBeamCursor.value;
public static final long sel_PMPrintSession = Selector.sel_PMPrintSession.value;
public static final long sel_PMPrintSettings = Selector.sel_PMPrintSettings.value;
public static final long sel_TIFFRepresentation = Selector.sel_TIFFRepresentation.value;
public static final long sel_URL = Selector.sel_URL.value;
public static final long sel_URLFromPasteboard_ = Selector.sel_URLFromPasteboard_.value;
public static final long sel_URLWithString_ = Selector.sel_URLWithString_.value;
public static final long sel_UTF8String = Selector.sel_UTF8String.value;
public static final long sel_abortEditing = Selector.sel_abortEditing.value;
public static final long sel_absoluteString = Selector.sel_absoluteString.value;
public static final long sel_acceptsFirstMouse_ = Selector.sel_acceptsFirstMouse_.value;
public static final long sel_acceptsFirstResponder = Selector.sel_acceptsFirstResponder.value;
public static final long sel_accessibilityActionDescription_ = Selector.sel_accessibilityActionDescription_.value;
public static final long sel_accessibilityActionNames = Selector.sel_accessibilityActionNames.value;
public static final long sel_accessibilityAttributeNames = Selector.sel_accessibilityAttributeNames.value;
public static final long sel_accessibilityAttributeValue_ = Selector.sel_accessibilityAttributeValue_.value;
public static final long sel_accessibilityAttributeValue_forParameter_ = Selector.sel_accessibilityAttributeValue_forParameter_.value;
public static final long sel_accessibilityFocusedUIElement = Selector.sel_accessibilityFocusedUIElement.value;
public static final long sel_accessibilityHitTest_ = Selector.sel_accessibilityHitTest_.value;
public static final long sel_accessibilityIsAttributeSettable_ = Selector.sel_accessibilityIsAttributeSettable_.value;
public static final long sel_accessibilityIsIgnored = Selector.sel_accessibilityIsIgnored.value;
public static final long sel_accessibilityParameterizedAttributeNames = Selector.sel_accessibilityParameterizedAttributeNames.value;
public static final long sel_accessibilityPerformAction_ = Selector.sel_accessibilityPerformAction_.value;
public static final long sel_accessibilitySetOverrideValue_forAttribute_ = Selector.sel_accessibilitySetOverrideValue_forAttribute_.value;
public static final long sel_accessibilitySetValue_forAttribute_ = Selector.sel_accessibilitySetValue_forAttribute_.value;
public static final long sel_action = Selector.sel_action.value;
public static final long sel_activateIgnoringOtherApps_ = Selector.sel_activateIgnoringOtherApps_.value;
public static final long sel_activateWithOptions_ = Selector.sel_activateWithOptions_.value;
public static final long sel_addAttribute_value_range_ = Selector.sel_addAttribute_value_range_.value;
public static final long sel_addButtonWithTitle_ = Selector.sel_addButtonWithTitle_.value;
public static final long sel_addChildWindow_ordered_ = Selector.sel_addChildWindow_ordered_.value;
public static final long sel_addClip = Selector.sel_addClip.value;
public static final long sel_addEventListener_listener_useCapture_ = Selector.sel_addEventListener_listener_useCapture_.value;
public static final long sel_addIndex_ = Selector.sel_addIndex_.value;
public static final long sel_addItem_ = Selector.sel_addItem_.value;
public static final long sel_addItemWithObjectValue_ = Selector.sel_addItemWithObjectValue_.value;
public static final long sel_addItemWithTitle_action_keyEquivalent_ = Selector.sel_addItemWithTitle_action_keyEquivalent_.value;
public static final long sel_addLayoutManager_ = Selector.sel_addLayoutManager_.value;
public static final long sel_addObject_ = Selector.sel_addObject_.value;
public static final long sel_addObjectsFromArray_ = Selector.sel_addObjectsFromArray_.value;
public static final long sel_addObserver_selector_name_object_ = Selector.sel_addObserver_selector_name_object_.value;
public static final long sel_addRepresentation_ = Selector.sel_addRepresentation_.value;
public static final long sel_addSubview_ = Selector.sel_addSubview_.value;
public static final long sel_addSubview_positioned_relativeTo_ = Selector.sel_addSubview_positioned_relativeTo_.value;
public static final long sel_addTabStop_ = Selector.sel_addTabStop_.value;
public static final long sel_addTableColumn_ = Selector.sel_addTableColumn_.value;
public static final long sel_addTemporaryAttribute_value_forCharacterRange_ = Selector.sel_addTemporaryAttribute_value_forCharacterRange_.value;
public static final long sel_addTextContainer_ = Selector.sel_addTextContainer_.value;
public static final long sel_addTimer_forMode_ = Selector.sel_addTimer_forMode_.value;
public static final long sel_addToolTipRect_owner_userData_ = Selector.sel_addToolTipRect_owner_userData_.value;
public static final long sel_addTypes_owner_ = Selector.sel_addTypes_owner_.value;
public static final long sel_alignment = Selector.sel_alignment.value;
public static final long sel_allKeys = Selector.sel_allKeys.value;
public static final long sel_allObjects = Selector.sel_allObjects.value;
public static final long sel_alloc = Selector.sel_alloc.value;
public static final long sel_alphaComponent = Selector.sel_alphaComponent.value;
public static final long sel_alphaValue = Selector.sel_alphaValue.value;
public static final long sel_altKey = Selector.sel_altKey.value;
public static final long sel_alternateSelectedControlColor = Selector.sel_alternateSelectedControlColor.value;
public static final long sel_alternateSelectedControlTextColor = Selector.sel_alternateSelectedControlTextColor.value;
public static final long sel_appearanceNamed_ = Selector.sel_appearanceNamed_.value;
public static final long sel_appendAttributedString_ = Selector.sel_appendAttributedString_.value;
public static final long sel_appendBezierPath_ = Selector.sel_appendBezierPath_.value;
public static final long sel_appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwise_ = Selector.sel_appendBezierPathWithArcWithCenter_radius_startAngle_endAngle_clockwise_.value;
public static final long sel_appendBezierPathWithGlyphs_count_inFont_ = Selector.sel_appendBezierPathWithGlyphs_count_inFont_.value;
public static final long sel_appendBezierPathWithOvalInRect_ = Selector.sel_appendBezierPathWithOvalInRect_.value;
public static final long sel_appendBezierPathWithRect_ = Selector.sel_appendBezierPathWithRect_.value;
public static final long sel_appendBezierPathWithRoundedRect_xRadius_yRadius_ = Selector.sel_appendBezierPathWithRoundedRect_xRadius_yRadius_.value;
public static final long sel_appendString_ = Selector.sel_appendString_.value;
public static final long sel_application_openFile_ = Selector.sel_application_openFile_.value;
public static final long sel_application_openFiles_ = Selector.sel_application_openFiles_.value;
public static final long sel_applicationDidBecomeActive_ = Selector.sel_applicationDidBecomeActive_.value;
public static final long sel_applicationDidResignActive_ = Selector.sel_applicationDidResignActive_.value;
public static final long sel_applicationDockMenu_ = Selector.sel_applicationDockMenu_.value;
public static final long sel_applicationIconImage = Selector.sel_applicationIconImage.value;
public static final long sel_applicationShouldHandleReopen_hasVisibleWindows_ = Selector.sel_applicationShouldHandleReopen_hasVisibleWindows_.value;
public static final long sel_applicationShouldTerminate_ = Selector.sel_applicationShouldTerminate_.value;
public static final long sel_applicationWillFinishLaunching_ = Selector.sel_applicationWillFinishLaunching_.value;
public static final long sel_archivedDataWithRootObject_ = Selector.sel_archivedDataWithRootObject_.value;
public static final long sel_areCursorRectsEnabled = Selector.sel_areCursorRectsEnabled.value;
public static final long sel_arrangeInFront_ = Selector.sel_arrangeInFront_.value;
public static final long sel_array = Selector.sel_array.value;
public static final long sel_arrayWithCapacity_ = Selector.sel_arrayWithCapacity_.value;
public static final long sel_arrayWithObject_ = Selector.sel_arrayWithObject_.value;
public static final long sel_arrowCursor = Selector.sel_arrowCursor.value;
public static final long sel_ascender = Selector.sel_ascender.value;
public static final long sel_attachColorList_ = Selector.sel_attachColorList_.value;
public static final long sel_attachment = Selector.sel_attachment.value;
public static final long sel_attribute_atIndex_effectiveRange_ = Selector.sel_attribute_atIndex_effectiveRange_.value;
public static final long sel_attributedStringValue = Selector.sel_attributedStringValue.value;
public static final long sel_attributedSubstringFromRange_ = Selector.sel_attributedSubstringFromRange_.value;
public static final long sel_attributedTitle = Selector.sel_attributedTitle.value;
public static final long sel_attributesAtIndex_longestEffectiveRange_inRange_ = Selector.sel_attributesAtIndex_longestEffectiveRange_inRange_.value;
public static final long sel_autorelease = Selector.sel_autorelease.value;
public static final long sel_availableFontFamilies = Selector.sel_availableFontFamilies.value;
public static final long sel_availableMembersOfFontFamily_ = Selector.sel_availableMembersOfFontFamily_.value;
public static final long sel_availableTypeFromArray_ = Selector.sel_availableTypeFromArray_.value;
public static final long sel_backingScaleFactor = Selector.sel_backingScaleFactor.value;
public static final long sel_badgeLabel = Selector.sel_badgeLabel.value;
public static final long sel_baselineOffsetInLayoutManager_glyphIndex_ = Selector.sel_baselineOffsetInLayoutManager_glyphIndex_.value;
public static final long sel_becomeFirstResponder = Selector.sel_becomeFirstResponder.value;
public static final long sel_becomeKeyWindow = Selector.sel_becomeKeyWindow.value;
public static final long sel_beginDocument = Selector.sel_beginDocument.value;
public static final long sel_beginEditing = Selector.sel_beginEditing.value;
public static final long sel_beginPageInRect_atPlacement_ = Selector.sel_beginPageInRect_atPlacement_.value;
public static final long sel_beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfo_ = Selector.sel_beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfo_.value;
public static final long sel_beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_message_ = Selector.sel_beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_message_.value;
public static final long sel_beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo_ = Selector.sel_beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo_.value;
public static final long sel_beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo_ = Selector.sel_beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo_.value;
public static final long sel_beginUndoGrouping = Selector.sel_beginUndoGrouping.value;
public static final long sel_bestRepresentationForDevice_ = Selector.sel_bestRepresentationForDevice_.value;
public static final long sel_bezelStyle = Selector.sel_bezelStyle.value;
public static final long sel_bezierPath = Selector.sel_bezierPath.value;
public static final long sel_bezierPathByFlatteningPath = Selector.sel_bezierPathByFlatteningPath.value;
public static final long sel_bezierPathWithRect_ = Selector.sel_bezierPathWithRect_.value;
public static final long sel_bezierPathWithRoundedRect_xRadius_yRadius_ = Selector.sel_bezierPathWithRoundedRect_xRadius_yRadius_.value;
public static final long sel_bitmapData = Selector.sel_bitmapData.value;
public static final long sel_bitmapFormat = Selector.sel_bitmapFormat.value;
public static final long sel_bitmapImageRepForCachingDisplayInRect_ = Selector.sel_bitmapImageRepForCachingDisplayInRect_.value;
public static final long sel_bitsPerPixel = Selector.sel_bitsPerPixel.value;
public static final long sel_bitsPerSample = Selector.sel_bitsPerSample.value;
public static final long sel_blackColor = Selector.sel_blackColor.value;
public static final long sel_blueComponent = Selector.sel_blueComponent.value;
public static final long sel_boldSystemFontOfSize_ = Selector.sel_boldSystemFontOfSize_.value;
public static final long sel_boolValue = Selector.sel_boolValue.value;
public static final long sel_borderWidth = Selector.sel_borderWidth.value;
public static final long sel_boundingRectForGlyphRange_inTextContainer_ = Selector.sel_boundingRectForGlyphRange_inTextContainer_.value;
public static final long sel_boundingRectWithSize_options_ = Selector.sel_boundingRectWithSize_options_.value;
public static final long sel_bounds = Selector.sel_bounds.value;
public static final long sel_bundleIdentifier = Selector.sel_bundleIdentifier.value;
public static final long sel_bundlePath = Selector.sel_bundlePath.value;
public static final long sel_bundleWithIdentifier_ = Selector.sel_bundleWithIdentifier_.value;
public static final long sel_bundleWithPath_ = Selector.sel_bundleWithPath_.value;
public static final long sel_button = Selector.sel_button.value;
public static final long sel_buttonNumber = Selector.sel_buttonNumber.value;
public static final long sel_bytes = Selector.sel_bytes.value;
public static final long sel_bytesPerRow = Selector.sel_bytesPerRow.value;
public static final long sel_cacheDisplayInRect_toBitmapImageRep_ = Selector.sel_cacheDisplayInRect_toBitmapImageRep_.value;
public static final long sel_calendarDate = Selector.sel_calendarDate.value;
public static final long sel_canBecomeKeyView = Selector.sel_canBecomeKeyView.value;
public static final long sel_canBecomeKeyWindow = Selector.sel_canBecomeKeyWindow.value;
public static final long sel_canDragRowsWithIndexes_atPoint_ = Selector.sel_canDragRowsWithIndexes_atPoint_.value;
public static final long sel_canGoBack = Selector.sel_canGoBack.value;
public static final long sel_canGoForward = Selector.sel_canGoForward.value;
public static final long sel_canRedo = Selector.sel_canRedo.value;
public static final long sel_canShowMIMEType_ = Selector.sel_canShowMIMEType_.value;
public static final long sel_canUndo = Selector.sel_canUndo.value;
public static final long sel_cancel = Selector.sel_cancel.value;
public static final long sel_cancelAuthenticationChallenge_ = Selector.sel_cancelAuthenticationChallenge_.value;
public static final long sel_cancelButtonCell = Selector.sel_cancelButtonCell.value;
public static final long sel_cancelButtonRectForBounds_ = Selector.sel_cancelButtonRectForBounds_.value;
public static final long sel_cancelOperation_ = Selector.sel_cancelOperation_.value;
public static final long sel_cancelTracking = Selector.sel_cancelTracking.value;
public static final long sel_cascadeTopLeftFromPoint_ = Selector.sel_cascadeTopLeftFromPoint_.value;
public static final long sel_cell = Selector.sel_cell.value;
public static final long sel_cellClass = Selector.sel_cellClass.value;
public static final long sel_cellSize = Selector.sel_cellSize.value;
public static final long sel_cellSizeForBounds_ = Selector.sel_cellSizeForBounds_.value;
public static final long sel_changeColor_ = Selector.sel_changeColor_.value;
public static final long sel_changeFont_ = Selector.sel_changeFont_.value;
public static final long sel_charCode = Selector.sel_charCode.value;
public static final long sel_characterAtIndex_ = Selector.sel_characterAtIndex_.value;
public static final long sel_characterIndexForGlyphAtIndex_ = Selector.sel_characterIndexForGlyphAtIndex_.value;
public static final long sel_characterIndexForInsertionAtPoint_ = Selector.sel_characterIndexForInsertionAtPoint_.value;
public static final long sel_characterIndexForPoint_ = Selector.sel_characterIndexForPoint_.value;
public static final long sel_characters = Selector.sel_characters.value;
public static final long sel_charactersIgnoringModifiers = Selector.sel_charactersIgnoringModifiers.value;
public static final long sel_chooseFilename_ = Selector.sel_chooseFilename_.value;
public static final long sel_className = Selector.sel_className.value;
public static final long sel_cleanUpOperation = Selector.sel_cleanUpOperation.value;
public static final long sel_clearColor = Selector.sel_clearColor.value;
public static final long sel_clearDrawable = Selector.sel_clearDrawable.value;
public static final long sel_clickCount = Selector.sel_clickCount.value;
public static final long sel_clickedColumn = Selector.sel_clickedColumn.value;
public static final long sel_clickedRow = Selector.sel_clickedRow.value;
public static final long sel_close = Selector.sel_close.value;
public static final long sel_closePath = Selector.sel_closePath.value;
public static final long sel_code = Selector.sel_code.value;
public static final long sel_collapseItem_ = Selector.sel_collapseItem_.value;
public static final long sel_collapseItem_collapseChildren_ = Selector.sel_collapseItem_collapseChildren_.value;
public static final long sel_collectionBehavior = Selector.sel_collectionBehavior.value;
public static final long sel_color = Selector.sel_color.value;
public static final long sel_colorAtX_y_ = Selector.sel_colorAtX_y_.value;
public static final long sel_colorListNamed_ = Selector.sel_colorListNamed_.value;
public static final long sel_colorSpace = Selector.sel_colorSpace.value;
public static final long sel_colorSpaceModel = Selector.sel_colorSpaceModel.value;
public static final long sel_colorUsingColorSpaceName_ = Selector.sel_colorUsingColorSpaceName_.value;
public static final long sel_colorWithDeviceRed_green_blue_alpha_ = Selector.sel_colorWithDeviceRed_green_blue_alpha_.value;
public static final long sel_colorWithKey_ = Selector.sel_colorWithKey_.value;
public static final long sel_colorWithPatternImage_ = Selector.sel_colorWithPatternImage_.value;
public static final long sel_columnAtPoint_ = Selector.sel_columnAtPoint_.value;
public static final long sel_comboBoxSelectionDidChange_ = Selector.sel_comboBoxSelectionDidChange_.value;
public static final long sel_comboBoxWillDismiss_ = Selector.sel_comboBoxWillDismiss_.value;
public static final long sel_comboBoxWillPopUp_ = Selector.sel_comboBoxWillPopUp_.value;
public static final long sel_compare_ = Selector.sel_compare_.value;
public static final long sel_concat = Selector.sel_concat.value;
public static final long sel_conformsToProtocol_ = Selector.sel_conformsToProtocol_.value;
public static final long sel_containsIndex_ = Selector.sel_containsIndex_.value;
public static final long sel_containsObject_ = Selector.sel_containsObject_.value;
public static final long sel_containsPoint_ = Selector.sel_containsPoint_.value;
public static final long sel_contentRect = Selector.sel_contentRect.value;
public static final long sel_contentSize = Selector.sel_contentSize.value;
public static final long sel_contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderType_ = Selector.sel_contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderType_.value;
public static final long sel_contentView = Selector.sel_contentView.value;
public static final long sel_contentViewMargins = Selector.sel_contentViewMargins.value;
public static final long sel_context = Selector.sel_context.value;
public static final long sel_controlBackgroundColor = Selector.sel_controlBackgroundColor.value;
public static final long sel_controlDarkShadowColor = Selector.sel_controlDarkShadowColor.value;
public static final long sel_controlHighlightColor = Selector.sel_controlHighlightColor.value;
public static final long sel_controlLightHighlightColor = Selector.sel_controlLightHighlightColor.value;
public static final long sel_controlPointBounds = Selector.sel_controlPointBounds.value;
public static final long sel_controlShadowColor = Selector.sel_controlShadowColor.value;
public static final long sel_controlSize = Selector.sel_controlSize.value;
public static final long sel_controlTextColor = Selector.sel_controlTextColor.value;
public static final long sel_convertBaseToScreen_ = Selector.sel_convertBaseToScreen_.value;
public static final long sel_convertFont_toHaveTrait_ = Selector.sel_convertFont_toHaveTrait_.value;
public static final long sel_convertPoint_fromView_ = Selector.sel_convertPoint_fromView_.value;
public static final long sel_convertPoint_toView_ = Selector.sel_convertPoint_toView_.value;
public static final long sel_convertRect_fromView_ = Selector.sel_convertRect_fromView_.value;
public static final long sel_convertRect_toView_ = Selector.sel_convertRect_toView_.value;
public static final long sel_convertScreenToBase_ = Selector.sel_convertScreenToBase_.value;
public static final long sel_cookies = Selector.sel_cookies.value;
public static final long sel_cookiesForURL_ = Selector.sel_cookiesForURL_.value;
public static final long sel_cookiesWithResponseHeaderFields_forURL_ = Selector.sel_cookiesWithResponseHeaderFields_forURL_.value;
public static final long sel_copiesOnScroll = Selector.sel_copiesOnScroll.value;
public static final long sel_copy = Selector.sel_copy.value;
public static final long sel_copy_ = Selector.sel_copy_.value;
public static final long sel_count = Selector.sel_count.value;
public static final long sel_createContext = Selector.sel_createContext.value;
public static final long sel_credentialWithUser_password_persistence_ = Selector.sel_credentialWithUser_password_persistence_.value;
public static final long sel_crosshairCursor = Selector.sel_crosshairCursor.value;
public static final long sel_ctrlKey = Selector.sel_ctrlKey.value;
public static final long sel_currentAppearance = Selector.sel_currentAppearance.value;
public static final long sel_currentApplication = Selector.sel_currentApplication.value;
public static final long sel_currentContext = Selector.sel_currentContext.value;
public static final long sel_currentCursor = Selector.sel_currentCursor.value;
public static final long sel_currentEditor = Selector.sel_currentEditor.value;
public static final long sel_currentEvent = Selector.sel_currentEvent.value;
public static final long sel_currentInputManager = Selector.sel_currentInputManager.value;
public static final long sel_currentPoint = Selector.sel_currentPoint.value;
public static final long sel_currentRunLoop = Selector.sel_currentRunLoop.value;
public static final long sel_currentThread = Selector.sel_currentThread.value;
public static final long sel_cursorUpdate_ = Selector.sel_cursorUpdate_.value;
public static final long sel_curveToPoint_controlPoint1_controlPoint2_ = Selector.sel_curveToPoint_controlPoint1_controlPoint2_.value;
public static final long sel_cut_ = Selector.sel_cut_.value;
public static final long sel_dataCell = Selector.sel_dataCell.value;
public static final long sel_dataForType_ = Selector.sel_dataForType_.value;
public static final long sel_dataSource = Selector.sel_dataSource.value;
public static final long sel_dataWithBytes_length_ = Selector.sel_dataWithBytes_length_.value;
public static final long sel_dateValue = Selector.sel_dateValue.value;
public static final long sel_dateWithCalendarFormat_timeZone_ = Selector.sel_dateWithCalendarFormat_timeZone_.value;
public static final long sel_dateWithTimeIntervalSinceNow_ = Selector.sel_dateWithTimeIntervalSinceNow_.value;
public static final long sel_dateWithYear_month_day_hour_minute_second_timeZone_ = Selector.sel_dateWithYear_month_day_hour_minute_second_timeZone_.value;
public static final long sel_dayOfMonth = Selector.sel_dayOfMonth.value;
public static final long sel_dealloc = Selector.sel_dealloc.value;
public static final long sel_decimalSeparator = Selector.sel_decimalSeparator.value;
public static final long sel_declareTypes_owner_ = Selector.sel_declareTypes_owner_.value;
public static final long sel_defaultBaselineOffsetForFont_ = Selector.sel_defaultBaselineOffsetForFont_.value;
public static final long sel_defaultButtonCell = Selector.sel_defaultButtonCell.value;
public static final long sel_defaultCenter = Selector.sel_defaultCenter.value;
public static final long sel_defaultFlatness = Selector.sel_defaultFlatness.value;
public static final long sel_defaultLineHeightForFont_ = Selector.sel_defaultLineHeightForFont_.value;
public static final long sel_defaultManager = Selector.sel_defaultManager.value;
public static final long sel_defaultParagraphStyle = Selector.sel_defaultParagraphStyle.value;
public static final long sel_defaultPrinter = Selector.sel_defaultPrinter.value;
public static final long sel_defaultTimeZone = Selector.sel_defaultTimeZone.value;
public static final long sel_delegate = Selector.sel_delegate.value;
public static final long sel_deleteCookie_ = Selector.sel_deleteCookie_.value;
public static final long sel_deliverResult = Selector.sel_deliverResult.value;
public static final long sel_deltaX = Selector.sel_deltaX.value;
public static final long sel_deltaY = Selector.sel_deltaY.value;
public static final long sel_deminiaturize_ = Selector.sel_deminiaturize_.value;
public static final long sel_depth = Selector.sel_depth.value;
public static final long sel_descender = Selector.sel_descender.value;
public static final long sel_description = Selector.sel_description.value;
public static final long sel_deselectAll_ = Selector.sel_deselectAll_.value;
public static final long sel_deselectItemAtIndex_ = Selector.sel_deselectItemAtIndex_.value;
public static final long sel_deselectRow_ = Selector.sel_deselectRow_.value;
public static final long sel_destroyContext = Selector.sel_destroyContext.value;
public static final long sel_detail = Selector.sel_detail.value;
public static final long sel_device = Selector.sel_device.value;
public static final long sel_deviceDescription = Selector.sel_deviceDescription.value;
public static final long sel_deviceSize = Selector.sel_deviceSize.value;
public static final long sel_dictionary = Selector.sel_dictionary.value;
public static final long sel_dictionaryWithCapacity_ = Selector.sel_dictionaryWithCapacity_.value;
public static final long sel_dictionaryWithObject_forKey_ = Selector.sel_dictionaryWithObject_forKey_.value;
public static final long sel_disableCursorRects = Selector.sel_disableCursorRects.value;
public static final long sel_disableFlushWindow = Selector.sel_disableFlushWindow.value;
public static final long sel_disabledControlTextColor = Selector.sel_disabledControlTextColor.value;
public static final long sel_discardCursorRects = Selector.sel_discardCursorRects.value;
public static final long sel_display = Selector.sel_display.value;
public static final long sel_displayIfNeeded = Selector.sel_displayIfNeeded.value;
public static final long sel_displayName = Selector.sel_displayName.value;
public static final long sel_displayNameForKey_value_ = Selector.sel_displayNameForKey_value_.value;
public static final long sel_displayRectIgnoringOpacity_inContext_ = Selector.sel_displayRectIgnoringOpacity_inContext_.value;
public static final long sel_distantFuture = Selector.sel_distantFuture.value;
public static final long sel_doCommandBySelector_ = Selector.sel_doCommandBySelector_.value;
public static final long sel_dockTile = Selector.sel_dockTile.value;
public static final long sel_documentSource = Selector.sel_documentSource.value;
public static final long sel_documentView = Selector.sel_documentView.value;
public static final long sel_documentViewShouldHandlePrint = Selector.sel_documentViewShouldHandlePrint.value;
public static final long sel_documentVisibleRect = Selector.sel_documentVisibleRect.value;
public static final long sel_doubleClickInterval = Selector.sel_doubleClickInterval.value;
public static final long sel_doubleValue = Selector.sel_doubleValue.value;
public static final long sel_download = Selector.sel_download.value;
public static final long sel_download_decideDestinationWithSuggestedFilename_ = Selector.sel_download_decideDestinationWithSuggestedFilename_.value;
public static final long sel_dragImage_at_offset_event_pasteboard_source_slideBack_ = Selector.sel_dragImage_at_offset_event_pasteboard_source_slideBack_.value;
public static final long sel_dragImageForRowsWithIndexes_tableColumns_event_offset_ = Selector.sel_dragImageForRowsWithIndexes_tableColumns_event_offset_.value;
public static final long sel_dragSelectionWithEvent_offset_slideBack_ = Selector.sel_dragSelectionWithEvent_offset_slideBack_.value;
public static final long sel_draggedImage_beganAt_ = Selector.sel_draggedImage_beganAt_.value;
public static final long sel_draggedImage_endedAt_operation_ = Selector.sel_draggedImage_endedAt_operation_.value;
public static final long sel_draggingDestinationWindow = Selector.sel_draggingDestinationWindow.value;
public static final long sel_draggingEntered_ = Selector.sel_draggingEntered_.value;
public static final long sel_draggingExited_ = Selector.sel_draggingExited_.value;
public static final long sel_draggingLocation = Selector.sel_draggingLocation.value;
public static final long sel_draggingPasteboard = Selector.sel_draggingPasteboard.value;
public static final long sel_draggingSourceOperationMask = Selector.sel_draggingSourceOperationMask.value;
public static final long sel_draggingSourceOperationMaskForLocal_ = Selector.sel_draggingSourceOperationMaskForLocal_.value;
public static final long sel_draggingUpdated_ = Selector.sel_draggingUpdated_.value;
public static final long sel_drawBackgroundForGlyphRange_atPoint_ = Selector.sel_drawBackgroundForGlyphRange_atPoint_.value;
public static final long sel_drawBackgroundInClipRect_ = Selector.sel_drawBackgroundInClipRect_.value;
public static final long sel_drawBezelWithFrame_inView_ = Selector.sel_drawBezelWithFrame_inView_.value;
public static final long sel_drawFromPoint_toPoint_options_ = Selector.sel_drawFromPoint_toPoint_options_.value;
public static final long sel_drawGlyphsForGlyphRange_atPoint_ = Selector.sel_drawGlyphsForGlyphRange_atPoint_.value;
public static final long sel_drawImage_withFrame_inView_ = Selector.sel_drawImage_withFrame_inView_.value;
public static final long sel_drawInBezierPath_angle_ = Selector.sel_drawInBezierPath_angle_.value;
public static final long sel_drawInRect_ = Selector.sel_drawInRect_.value;
public static final long sel_drawInRect_angle_ = Selector.sel_drawInRect_angle_.value;
public static final long sel_drawInRect_fromRect_operation_fraction_ = Selector.sel_drawInRect_fromRect_operation_fraction_.value;
public static final long sel_drawInteriorWithFrame_inView_ = Selector.sel_drawInteriorWithFrame_inView_.value;
public static final long sel_drawLabel_inRect_ = Selector.sel_drawLabel_inRect_.value;
public static final long sel_drawRect_ = Selector.sel_drawRect_.value;
public static final long sel_drawSortIndicatorWithFrame_inView_ascending_priority_ = Selector.sel_drawSortIndicatorWithFrame_inView_ascending_priority_.value;
public static final long sel_drawStatusBarBackgroundInRect_withHighlight_ = Selector.sel_drawStatusBarBackgroundInRect_withHighlight_.value;
public static final long sel_drawTitle_withFrame_inView_ = Selector.sel_drawTitle_withFrame_inView_.value;
public static final long sel_drawViewBackgroundInRect_ = Selector.sel_drawViewBackgroundInRect_.value;
public static final long sel_drawWithExpansionFrame_inView_ = Selector.sel_drawWithExpansionFrame_inView_.value;
public static final long sel_drawingRectForBounds_ = Selector.sel_drawingRectForBounds_.value;
public static final long sel_elementAtIndex_associatedPoints_ = Selector.sel_elementAtIndex_associatedPoints_.value;
public static final long sel_elementCount = Selector.sel_elementCount.value;
public static final long sel_enableCursorRects = Selector.sel_enableCursorRects.value;
public static final long sel_enableFlushWindow = Selector.sel_enableFlushWindow.value;
public static final long sel_endDocument = Selector.sel_endDocument.value;
public static final long sel_endEditing = Selector.sel_endEditing.value;
public static final long sel_endEditingFor_ = Selector.sel_endEditingFor_.value;
public static final long sel_endPage = Selector.sel_endPage.value;
public static final long sel_endSheet_returnCode_ = Selector.sel_endSheet_returnCode_.value;
public static final long sel_endUndoGrouping = Selector.sel_endUndoGrouping.value;
public static final long sel_enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userData_ = Selector.sel_enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userData_.value;
public static final long sel_enumeratorAtPath_ = Selector.sel_enumeratorAtPath_.value;
public static final long sel_expandItem_ = Selector.sel_expandItem_.value;
public static final long sel_expandItem_expandChildren_ = Selector.sel_expandItem_expandChildren_.value;
public static final long sel_expansionFrameWithFrame_inView_ = Selector.sel_expansionFrameWithFrame_inView_.value;
public static final long sel_familyName = Selector.sel_familyName.value;
public static final long sel_fieldEditor_forObject_ = Selector.sel_fieldEditor_forObject_.value;
public static final long sel_fileExistsAtPath_ = Selector.sel_fileExistsAtPath_.value;
public static final long sel_fileExistsAtPath_isDirectory_ = Selector.sel_fileExistsAtPath_isDirectory_.value;
public static final long sel_fileURLWithPath_ = Selector.sel_fileURLWithPath_.value;
public static final long sel_filename = Selector.sel_filename.value;
public static final long sel_filenames = Selector.sel_filenames.value;
public static final long sel_fill = Selector.sel_fill.value;
public static final long sel_fillRect_ = Selector.sel_fillRect_.value;
public static final long sel_finishLaunching = Selector.sel_finishLaunching.value;
public static final long sel_firstRectForCharacterRange_ = Selector.sel_firstRectForCharacterRange_.value;
public static final long sel_firstResponder = Selector.sel_firstResponder.value;
public static final long sel_flagsChanged_ = Selector.sel_flagsChanged_.value;
public static final long sel_flashScrollers = Selector.sel_flashScrollers.value;
public static final long sel_floatValue = Selector.sel_floatValue.value;
public static final long sel_flushBuffer = Selector.sel_flushBuffer.value;
public static final long sel_flushGraphics = Selector.sel_flushGraphics.value;
public static final long sel_flushWindowIfNeeded = Selector.sel_flushWindowIfNeeded.value;
public static final long sel_focusRingMaskBoundsForFrame_inView_ = Selector.sel_focusRingMaskBoundsForFrame_inView_.value;
public static final long sel_font = Selector.sel_font.value;
public static final long sel_fontName = Selector.sel_fontName.value;
public static final long sel_fontWithName_size_ = Selector.sel_fontWithName_size_.value;
public static final long sel_frame = Selector.sel_frame.value;
public static final long sel_frameOfCellAtColumn_row_ = Selector.sel_frameOfCellAtColumn_row_.value;
public static final long sel_frameOfOutlineCellAtRow_ = Selector.sel_frameOfOutlineCellAtRow_.value;
public static final long sel_frameRectForContentRect_ = Selector.sel_frameRectForContentRect_.value;
public static final long sel_frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderType_ = Selector.sel_frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderType_.value;
public static final long sel_frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle_ = Selector.sel_frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle_.value;
public static final long sel_fullPathForApplication_ = Selector.sel_fullPathForApplication_.value;
public static final long sel_generalPasteboard = Selector.sel_generalPasteboard.value;
public static final long sel_getBytes_ = Selector.sel_getBytes_.value;
public static final long sel_getCharacters_ = Selector.sel_getCharacters_.value;
public static final long sel_getCharacters_range_ = Selector.sel_getCharacters_range_.value;
public static final long sel_getComponents_ = Selector.sel_getComponents_.value;
public static final long sel_getGlyphs_range_ = Selector.sel_getGlyphs_range_.value;
public static final long sel_getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevels_ = Selector.sel_getGlyphsInRange_glyphs_characterIndexes_glyphInscriptions_elasticBits_bidiLevels_.value;
public static final long sel_getIndexes_maxCount_inIndexRange_ = Selector.sel_getIndexes_maxCount_inIndexRange_.value;
public static final long sel_getValues_forAttribute_forVirtualScreen_ = Selector.sel_getValues_forAttribute_forVirtualScreen_.value;
public static final long sel_globalContext = Selector.sel_globalContext.value;
public static final long sel_glyphIndexForCharacterAtIndex_ = Selector.sel_glyphIndexForCharacterAtIndex_.value;
public static final long sel_glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyph_ = Selector.sel_glyphIndexForPoint_inTextContainer_fractionOfDistanceThroughGlyph_.value;
public static final long sel_glyphRangeForCharacterRange_actualCharacterRange_ = Selector.sel_glyphRangeForCharacterRange_actualCharacterRange_.value;
public static final long sel_glyphRangeForTextContainer_ = Selector.sel_glyphRangeForTextContainer_.value;
public static final long sel_goBack = Selector.sel_goBack.value;
public static final long sel_goForward = Selector.sel_goForward.value;
public static final long sel_graphicsContext = Selector.sel_graphicsContext.value;
public static final long sel_graphicsContextWithBitmapImageRep_ = Selector.sel_graphicsContextWithBitmapImageRep_.value;
public static final long sel_graphicsContextWithGraphicsPort_flipped_ = Selector.sel_graphicsContextWithGraphicsPort_flipped_.value;
public static final long sel_graphicsContextWithWindow_ = Selector.sel_graphicsContextWithWindow_.value;
public static final long sel_graphicsPort = Selector.sel_graphicsPort.value;
public static final long sel_greenComponent = Selector.sel_greenComponent.value;
public static final long sel_handleEvent_ = Selector.sel_handleEvent_.value;
public static final long sel_handleMouseEvent_ = Selector.sel_handleMouseEvent_.value;
public static final long sel_hasAlpha = Selector.sel_hasAlpha.value;
public static final long sel_hasMarkedText = Selector.sel_hasMarkedText.value;
public static final long sel_hasPassword = Selector.sel_hasPassword.value;
public static final long sel_hasShadow = Selector.sel_hasShadow.value;
public static final long sel_headerCell = Selector.sel_headerCell.value;
public static final long sel_headerRectOfColumn_ = Selector.sel_headerRectOfColumn_.value;
public static final long sel_headerView = Selector.sel_headerView.value;
public static final long sel_helpRequested_ = Selector.sel_helpRequested_.value;
public static final long sel_hide_ = Selector.sel_hide_.value;
public static final long sel_hideOtherApplications_ = Selector.sel_hideOtherApplications_.value;
public static final long sel_highlightSelectionInClipRect_ = Selector.sel_highlightSelectionInClipRect_.value;
public static final long sel_hitPart = Selector.sel_hitPart.value;
public static final long sel_hitTest_ = Selector.sel_hitTest_.value;
public static final long sel_hitTestForEvent_inRect_ofView_ = Selector.sel_hitTestForEvent_inRect_ofView_.value;
public static final long sel_host = Selector.sel_host.value;
public static final long sel_hourOfDay = Selector.sel_hourOfDay.value;
public static final long sel_iconForFile_ = Selector.sel_iconForFile_.value;
public static final long sel_ignore = Selector.sel_ignore.value;
public static final long sel_ignoreModifierKeysWhileDragging = Selector.sel_ignoreModifierKeysWhileDragging.value;
public static final long sel_image = Selector.sel_image.value;
public static final long sel_imageInterpolation = Selector.sel_imageInterpolation.value;
public static final long sel_imageNamed_ = Selector.sel_imageNamed_.value;
public static final long sel_imageRectForBounds_ = Selector.sel_imageRectForBounds_.value;
public static final long sel_imageRepWithContentsOfFile_ = Selector.sel_imageRepWithContentsOfFile_.value;
public static final long sel_imageablePageBounds = Selector.sel_imageablePageBounds.value;
public static final long sel_increment = Selector.sel_increment.value;
public static final long sel_indentationPerLevel = Selector.sel_indentationPerLevel.value;
public static final long sel_indexOfItemWithTarget_andAction_ = Selector.sel_indexOfItemWithTarget_andAction_.value;
public static final long sel_indexOfObjectIdenticalTo_ = Selector.sel_indexOfObjectIdenticalTo_.value;
public static final long sel_indexOfSelectedItem = Selector.sel_indexOfSelectedItem.value;
public static final long sel_infoDictionary = Selector.sel_infoDictionary.value;
public static final long sel_init = Selector.sel_init.value;
public static final long sel_initByReferencingFile_ = Selector.sel_initByReferencingFile_.value;
public static final long sel_initListDescriptor = Selector.sel_initListDescriptor.value;
public static final long sel_initWithAttributes_ = Selector.sel_initWithAttributes_.value;
public static final long sel_initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel_ = Selector.sel_initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel_.value;
public static final long sel_initWithCapacity_ = Selector.sel_initWithCapacity_.value;
public static final long sel_initWithCharacters_length_ = Selector.sel_initWithCharacters_length_.value;
public static final long sel_initWithColors_ = Selector.sel_initWithColors_.value;
public static final long sel_initWithContainerSize_ = Selector.sel_initWithContainerSize_.value;
public static final long sel_initWithContentRect_styleMask_backing_defer_ = Selector.sel_initWithContentRect_styleMask_backing_defer_.value;
public static final long sel_initWithContentRect_styleMask_backing_defer_screen_ = Selector.sel_initWithContentRect_styleMask_backing_defer_screen_.value;
public static final long sel_initWithContentsOfFile_ = Selector.sel_initWithContentsOfFile_.value;
public static final long sel_initWithData_ = Selector.sel_initWithData_.value;
public static final long sel_initWithFileWrapper_ = Selector.sel_initWithFileWrapper_.value;
public static final long sel_initWithFormat_shareContext_ = Selector.sel_initWithFormat_shareContext_.value;
public static final long sel_initWithFrame_ = Selector.sel_initWithFrame_.value;
public static final long sel_initWithFrame_frameName_groupName_ = Selector.sel_initWithFrame_frameName_groupName_.value;
public static final long sel_initWithFrame_pullsDown_ = Selector.sel_initWithFrame_pullsDown_.value;
public static final long sel_initWithIconRef_ = Selector.sel_initWithIconRef_.value;
public static final long sel_initWithIdentifier_ = Selector.sel_initWithIdentifier_.value;
public static final long sel_initWithImage_hotSpot_ = Selector.sel_initWithImage_hotSpot_.value;
public static final long sel_initWithIndex_ = Selector.sel_initWithIndex_.value;
public static final long sel_initWithIndexesInRange_ = Selector.sel_initWithIndexesInRange_.value;
public static final long sel_initWithItemIdentifier_ = Selector.sel_initWithItemIdentifier_.value;
public static final long sel_initWithLocaleIdentifier_ = Selector.sel_initWithLocaleIdentifier_.value;
public static final long sel_initWithName_ = Selector.sel_initWithName_.value;
public static final long sel_initWithSize_ = Selector.sel_initWithSize_.value;
public static final long sel_initWithStartingColor_endingColor_ = Selector.sel_initWithStartingColor_endingColor_.value;
public static final long sel_initWithString_ = Selector.sel_initWithString_.value;
public static final long sel_initWithString_attributes_ = Selector.sel_initWithString_attributes_.value;
public static final long sel_initWithTitle_ = Selector.sel_initWithTitle_.value;
public static final long sel_initWithTitle_action_keyEquivalent_ = Selector.sel_initWithTitle_action_keyEquivalent_.value;
public static final long sel_initWithTransform_ = Selector.sel_initWithTransform_.value;
public static final long sel_initWithType_location_ = Selector.sel_initWithType_location_.value;
public static final long sel_insertColor_key_atIndex_ = Selector.sel_insertColor_key_atIndex_.value;
public static final long sel_insertItem_atIndex_ = Selector.sel_insertItem_atIndex_.value;
public static final long sel_insertItemWithItemIdentifier_atIndex_ = Selector.sel_insertItemWithItemIdentifier_atIndex_.value;
public static final long sel_insertItemWithObjectValue_atIndex_ = Selector.sel_insertItemWithObjectValue_atIndex_.value;
public static final long sel_insertTabViewItem_atIndex_ = Selector.sel_insertTabViewItem_atIndex_.value;
public static final long sel_insertText_ = Selector.sel_insertText_.value;
public static final long sel_insertText_replacementRange_ = Selector.sel_insertText_replacementRange_.value;
public static final long sel_intValue = Selector.sel_intValue.value;
public static final long sel_integerValue = Selector.sel_integerValue.value;
public static final long sel_intercellSpacing = Selector.sel_intercellSpacing.value;
public static final long sel_interpretKeyEvents_ = Selector.sel_interpretKeyEvents_.value;
public static final long sel_invalidate = Selector.sel_invalidate.value;
public static final long sel_invalidateShadow = Selector.sel_invalidateShadow.value;
public static final long sel_invert = Selector.sel_invert.value;
public static final long sel_isActive = Selector.sel_isActive.value;
public static final long sel_isCompatibleWithOverlayScrollers = Selector.sel_isCompatibleWithOverlayScrollers.value;
public static final long sel_isDescendantOf_ = Selector.sel_isDescendantOf_.value;
public static final long sel_isDocumentEdited = Selector.sel_isDocumentEdited.value;
public static final long sel_isDrawingToScreen = Selector.sel_isDrawingToScreen.value;
public static final long sel_isEmpty = Selector.sel_isEmpty.value;
public static final long sel_isEnabled = Selector.sel_isEnabled.value;
public static final long sel_isEqual_ = Selector.sel_isEqual_.value;
public static final long sel_isEqualToString_ = Selector.sel_isEqualToString_.value;
public static final long sel_isExecutableFileAtPath_ = Selector.sel_isExecutableFileAtPath_.value;
public static final long sel_isFieldEditor = Selector.sel_isFieldEditor.value;
public static final long sel_isFilePackageAtPath_ = Selector.sel_isFilePackageAtPath_.value;
public static final long sel_isFileURL = Selector.sel_isFileURL.value;
public static final long sel_isFlipped = Selector.sel_isFlipped.value;
public static final long sel_isHidden = Selector.sel_isHidden.value;
public static final long sel_isHiddenOrHasHiddenAncestor = Selector.sel_isHiddenOrHasHiddenAncestor.value;
public static final long sel_isHighlighted = Selector.sel_isHighlighted.value;
public static final long sel_isItemExpanded_ = Selector.sel_isItemExpanded_.value;
public static final long sel_isKeyWindow = Selector.sel_isKeyWindow.value;
public static final long sel_isKindOfClass_ = Selector.sel_isKindOfClass_.value;
public static final long sel_isMainThread = Selector.sel_isMainThread.value;
public static final long sel_isMainWindow = Selector.sel_isMainWindow.value;
public static final long sel_isMiniaturized = Selector.sel_isMiniaturized.value;
public static final long sel_isOpaque = Selector.sel_isOpaque.value;
public static final long sel_isPlanar = Selector.sel_isPlanar.value;
public static final long sel_isResting = Selector.sel_isResting.value;
public static final long sel_isRowSelected_ = Selector.sel_isRowSelected_.value;
public static final long sel_isRunning = Selector.sel_isRunning.value;
public static final long sel_isSelectionOnly = Selector.sel_isSelectionOnly.value;
public static final long sel_isSeparatorItem = Selector.sel_isSeparatorItem.value;
public static final long sel_isSessionOnly = Selector.sel_isSessionOnly.value;
public static final long sel_isSheet = Selector.sel_isSheet.value;
public static final long sel_isVisible = Selector.sel_isVisible.value;
public static final long sel_isZoomed = Selector.sel_isZoomed.value;
public static final long sel_itemArray = Selector.sel_itemArray.value;
public static final long sel_itemAtIndex_ = Selector.sel_itemAtIndex_.value;
public static final long sel_itemAtRow_ = Selector.sel_itemAtRow_.value;
public static final long sel_itemHeight = Selector.sel_itemHeight.value;
public static final long sel_itemIdentifier = Selector.sel_itemIdentifier.value;
public static final long sel_itemObjectValueAtIndex_ = Selector.sel_itemObjectValueAtIndex_.value;
public static final long sel_itemTitleAtIndex_ = Selector.sel_itemTitleAtIndex_.value;
public static final long sel_itemWithTag_ = Selector.sel_itemWithTag_.value;
public static final long sel_jobDisposition = Selector.sel_jobDisposition.value;
public static final long sel_keyCode = Selector.sel_keyCode.value;
public static final long sel_keyDown_ = Selector.sel_keyDown_.value;
public static final long sel_keyEquivalent = Selector.sel_keyEquivalent.value;
public static final long sel_keyEquivalentModifierMask = Selector.sel_keyEquivalentModifierMask.value;
public static final long sel_keyUp_ = Selector.sel_keyUp_.value;
public static final long sel_keyWindow = Selector.sel_keyWindow.value;
public static final long sel_knobProportion = Selector.sel_knobProportion.value;
public static final long sel_knobThickness = Selector.sel_knobThickness.value;
public static final long sel_lastPathComponent = Selector.sel_lastPathComponent.value;
public static final long sel_layoutManager = Selector.sel_layoutManager.value;
public static final long sel_leading = Selector.sel_leading.value;
public static final long sel_length = Selector.sel_length.value;
public static final long sel_levelForItem_ = Selector.sel_levelForItem_.value;
public static final long sel_lineFragmentUsedRectForGlyphAtIndex_effectiveRange_ = Selector.sel_lineFragmentUsedRectForGlyphAtIndex_effectiveRange_.value;
public static final long sel_lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout_ = Selector.sel_lineFragmentUsedRectForGlyphAtIndex_effectiveRange_withoutAdditionalLayout_.value;
public static final long sel_lineToPoint_ = Selector.sel_lineToPoint_.value;
public static final long sel_linkTextAttributes = Selector.sel_linkTextAttributes.value;
public static final long sel_loadHTMLString_baseURL_ = Selector.sel_loadHTMLString_baseURL_.value;
public static final long sel_loadNibFile_externalNameTable_withZone_ = Selector.sel_loadNibFile_externalNameTable_withZone_.value;
public static final long sel_loadRequest_ = Selector.sel_loadRequest_.value;
public static final long sel_localizedDescription = Selector.sel_localizedDescription.value;
public static final long sel_locationForGlyphAtIndex_ = Selector.sel_locationForGlyphAtIndex_.value;
public static final long sel_locationInWindow = Selector.sel_locationInWindow.value;
public static final long sel_lockFocus = Selector.sel_lockFocus.value;
public static final long sel_lockFocusIfCanDraw = Selector.sel_lockFocusIfCanDraw.value;
public static final long sel_lowercaseString = Selector.sel_lowercaseString.value;
public static final long sel_magnification = Selector.sel_magnification.value;
public static final long sel_magnifyWithEvent_ = Selector.sel_magnifyWithEvent_.value;
public static final long sel_mainBundle = Selector.sel_mainBundle.value;
public static final long sel_mainFrame = Selector.sel_mainFrame.value;
public static final long sel_mainMenu = Selector.sel_mainMenu.value;
public static final long sel_mainScreen = Selector.sel_mainScreen.value;
public static final long sel_makeCurrentContext = Selector.sel_makeCurrentContext.value;
public static final long sel_makeFirstResponder_ = Selector.sel_makeFirstResponder_.value;
public static final long sel_makeKeyAndOrderFront_ = Selector.sel_makeKeyAndOrderFront_.value;
public static final long sel_markedRange = Selector.sel_markedRange.value;
public static final long sel_markedTextAttributes = Selector.sel_markedTextAttributes.value;
public static final long sel_maxValue = Selector.sel_maxValue.value;
public static final long sel_menu = Selector.sel_menu.value;
public static final long sel_menu_willHighlightItem_ = Selector.sel_menu_willHighlightItem_.value;
public static final long sel_menuBarFontOfSize_ = Selector.sel_menuBarFontOfSize_.value;
public static final long sel_menuDidClose_ = Selector.sel_menuDidClose_.value;
public static final long sel_menuForEvent_ = Selector.sel_menuForEvent_.value;
public static final long sel_menuNeedsUpdate_ = Selector.sel_menuNeedsUpdate_.value;
public static final long sel_menuWillOpen_ = Selector.sel_menuWillOpen_.value;
public static final long sel_metaKey = Selector.sel_metaKey.value;
public static final long sel_minFrameWidthWithTitle_styleMask_ = Selector.sel_minFrameWidthWithTitle_styleMask_.value;
public static final long sel_minSize = Selector.sel_minSize.value;
public static final long sel_minValue = Selector.sel_minValue.value;
public static final long sel_miniaturize_ = Selector.sel_miniaturize_.value;
public static final long sel_minimumSize = Selector.sel_minimumSize.value;
public static final long sel_minuteOfHour = Selector.sel_minuteOfHour.value;
public static final long sel_modifierFlags = Selector.sel_modifierFlags.value;
public static final long sel_monthOfYear = Selector.sel_monthOfYear.value;
public static final long sel_mouse_inRect_ = Selector.sel_mouse_inRect_.value;
public static final long sel_mouseDown_ = Selector.sel_mouseDown_.value;
public static final long sel_mouseDownCanMoveWindow = Selector.sel_mouseDownCanMoveWindow.value;
public static final long sel_mouseDragged_ = Selector.sel_mouseDragged_.value;
public static final long sel_mouseEntered_ = Selector.sel_mouseEntered_.value;
public static final long sel_mouseExited_ = Selector.sel_mouseExited_.value;
public static final long sel_mouseLocation = Selector.sel_mouseLocation.value;
public static final long sel_mouseLocationOutsideOfEventStream = Selector.sel_mouseLocationOutsideOfEventStream.value;
public static final long sel_mouseMoved_ = Selector.sel_mouseMoved_.value;
public static final long sel_mouseUp_ = Selector.sel_mouseUp_.value;
public static final long sel_moveColumn_toColumn_ = Selector.sel_moveColumn_toColumn_.value;
public static final long sel_moveToPoint_ = Selector.sel_moveToPoint_.value;
public static final long sel_mutableCopy = Selector.sel_mutableCopy.value;
public static final long sel_name = Selector.sel_name.value;
public static final long sel_nameFieldStringValue = Selector.sel_nameFieldStringValue.value;
public static final long sel_needsPanelToBecomeKey = Selector.sel_needsPanelToBecomeKey.value;
public static final long sel_nextEventMatchingMask_untilDate_inMode_dequeue_ = Selector.sel_nextEventMatchingMask_untilDate_inMode_dequeue_.value;
public static final long sel_nextObject = Selector.sel_nextObject.value;
public static final long sel_nextState = Selector.sel_nextState.value;
public static final long sel_nextWordFromIndex_forward_ = Selector.sel_nextWordFromIndex_forward_.value;
public static final long sel_noResponderFor_ = Selector.sel_noResponderFor_.value;
public static final long sel_normalizedPosition = Selector.sel_normalizedPosition.value;
public static final long sel_noteNumberOfRowsChanged = Selector.sel_noteNumberOfRowsChanged.value;
public static final long sel_numberOfColumns = Selector.sel_numberOfColumns.value;
public static final long sel_numberOfComponents = Selector.sel_numberOfComponents.value;
public static final long sel_numberOfGlyphs = Selector.sel_numberOfGlyphs.value;
public static final long sel_numberOfItems = Selector.sel_numberOfItems.value;
public static final long sel_numberOfRows = Selector.sel_numberOfRows.value;
public static final long sel_numberOfRowsInTableView_ = Selector.sel_numberOfRowsInTableView_.value;
public static final long sel_numberOfSelectedRows = Selector.sel_numberOfSelectedRows.value;
public static final long sel_numberOfVisibleItems = Selector.sel_numberOfVisibleItems.value;
public static final long sel_numberWithBool_ = Selector.sel_numberWithBool_.value;
public static final long sel_numberWithDouble_ = Selector.sel_numberWithDouble_.value;
public static final long sel_numberWithInt_ = Selector.sel_numberWithInt_.value;
public static final long sel_numberWithInteger_ = Selector.sel_numberWithInteger_.value;
public static final long sel_objCType = Selector.sel_objCType.value;
public static final long sel_object = Selector.sel_object.value;
public static final long sel_objectAtIndex_ = Selector.sel_objectAtIndex_.value;
public static final long sel_objectEnumerator = Selector.sel_objectEnumerator.value;
public static final long sel_objectForInfoDictionaryKey_ = Selector.sel_objectForInfoDictionaryKey_.value;
public static final long sel_objectForKey_ = Selector.sel_objectForKey_.value;
public static final long sel_objectValues = Selector.sel_objectValues.value;
public static final long sel_openPanel = Selector.sel_openPanel.value;
public static final long sel_openURL_ = Selector.sel_openURL_.value;
public static final long sel_openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiers_ = Selector.sel_openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiers_.value;
public static final long sel_operatingSystemVersion = Selector.sel_operatingSystemVersion.value;
public static final long sel_operationNotAllowedCursor = Selector.sel_operationNotAllowedCursor.value;
public static final long sel_options = Selector.sel_options.value;
public static final long sel_orderBack_ = Selector.sel_orderBack_.value;
public static final long sel_orderFront_ = Selector.sel_orderFront_.value;
public static final long sel_orderFrontRegardless = Selector.sel_orderFrontRegardless.value;
public static final long sel_orderFrontStandardAboutPanel_ = Selector.sel_orderFrontStandardAboutPanel_.value;
public static final long sel_orderOut_ = Selector.sel_orderOut_.value;
public static final long sel_orderWindow_relativeTo_ = Selector.sel_orderWindow_relativeTo_.value;
public static final long sel_otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2_ = Selector.sel_otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2_.value;
public static final long sel_otherMouseDown_ = Selector.sel_otherMouseDown_.value;
public static final long sel_otherMouseDragged_ = Selector.sel_otherMouseDragged_.value;
public static final long sel_otherMouseUp_ = Selector.sel_otherMouseUp_.value;
public static final long sel_outlineView_acceptDrop_item_childIndex_ = Selector.sel_outlineView_acceptDrop_item_childIndex_.value;
public static final long sel_outlineView_child_ofItem_ = Selector.sel_outlineView_child_ofItem_.value;
public static final long sel_outlineView_didClickTableColumn_ = Selector.sel_outlineView_didClickTableColumn_.value;
public static final long sel_outlineView_isItemExpandable_ = Selector.sel_outlineView_isItemExpandable_.value;
public static final long sel_outlineView_numberOfChildrenOfItem_ = Selector.sel_outlineView_numberOfChildrenOfItem_.value;
public static final long sel_outlineView_objectValueForTableColumn_byItem_ = Selector.sel_outlineView_objectValueForTableColumn_byItem_.value;
public static final long sel_outlineView_setObjectValue_forTableColumn_byItem_ = Selector.sel_outlineView_setObjectValue_forTableColumn_byItem_.value;
public static final long sel_outlineView_shouldEditTableColumn_item_ = Selector.sel_outlineView_shouldEditTableColumn_item_.value;
public static final long sel_outlineView_shouldExpandItem_ = Selector.sel_outlineView_shouldExpandItem_.value;
public static final long sel_outlineView_shouldReorderColumn_toColumn_ = Selector.sel_outlineView_shouldReorderColumn_toColumn_.value;
public static final long sel_outlineView_shouldTrackCell_forTableColumn_item_ = Selector.sel_outlineView_shouldTrackCell_forTableColumn_item_.value;
public static final long sel_outlineView_validateDrop_proposedItem_proposedChildIndex_ = Selector.sel_outlineView_validateDrop_proposedItem_proposedChildIndex_.value;
public static final long sel_outlineView_willDisplayCell_forTableColumn_item_ = Selector.sel_outlineView_willDisplayCell_forTableColumn_item_.value;
public static final long sel_outlineView_writeItems_toPasteboard_ = Selector.sel_outlineView_writeItems_toPasteboard_.value;
public static final long sel_outlineViewColumnDidMove_ = Selector.sel_outlineViewColumnDidMove_.value;
public static final long sel_outlineViewColumnDidResize_ = Selector.sel_outlineViewColumnDidResize_.value;
public static final long sel_outlineViewSelectionDidChange_ = Selector.sel_outlineViewSelectionDidChange_.value;
public static final long sel_outlineViewSelectionIsChanging_ = Selector.sel_outlineViewSelectionIsChanging_.value;
public static final long sel_owner = Selector.sel_owner.value;
public static final long sel_pageDown_ = Selector.sel_pageDown_.value;
public static final long sel_pageTitle = Selector.sel_pageTitle.value;
public static final long sel_pageUp_ = Selector.sel_pageUp_.value;
public static final long sel_panel_shouldEnableURL_ = Selector.sel_panel_shouldEnableURL_.value;
public static final long sel_panel_userEnteredFilename_confirmed_ = Selector.sel_panel_userEnteredFilename_confirmed_.value;
public static final long sel_panelConvertFont_ = Selector.sel_panelConvertFont_.value;
public static final long sel_paperSize = Selector.sel_paperSize.value;
public static final long sel_paragraphs = Selector.sel_paragraphs.value;
public static final long sel_parentWindow = Selector.sel_parentWindow.value;
public static final long sel_password = Selector.sel_password.value;
public static final long sel_paste_ = Selector.sel_paste_.value;
public static final long sel_pasteboard_provideDataForType_ = Selector.sel_pasteboard_provideDataForType_.value;
public static final long sel_pasteboardWithName_ = Selector.sel_pasteboardWithName_.value;
public static final long sel_path = Selector.sel_path.value;
public static final long sel_pathExtension = Selector.sel_pathExtension.value;
public static final long sel_pathForResource_ofType_ = Selector.sel_pathForResource_ofType_.value;
public static final long sel_pathForResource_ofType_inDirectory_forLocalization_ = Selector.sel_pathForResource_ofType_inDirectory_forLocalization_.value;
public static final long sel_performDragOperation_ = Selector.sel_performDragOperation_.value;
public static final long sel_performKeyEquivalent_ = Selector.sel_performKeyEquivalent_.value;
public static final long sel_performSelector_withObject_afterDelay_inModes_ = Selector.sel_performSelector_withObject_afterDelay_inModes_.value;
public static final long sel_performSelectorOnMainThread_withObject_waitUntilDone_ = Selector.sel_performSelectorOnMainThread_withObject_waitUntilDone_.value;
public static final long sel_phase = Selector.sel_phase.value;
public static final long sel_pixelsHigh = Selector.sel_pixelsHigh.value;
public static final long sel_pixelsWide = Selector.sel_pixelsWide.value;
public static final long sel_pointSize = Selector.sel_pointSize.value;
public static final long sel_pointValue = Selector.sel_pointValue.value;
public static final long sel_pointingHandCursor = Selector.sel_pointingHandCursor.value;
public static final long sel_pop = Selector.sel_pop.value;
public static final long sel_popUpContextMenu_withEvent_forView_ = Selector.sel_popUpContextMenu_withEvent_forView_.value;
public static final long sel_popUpStatusItemMenu_ = Selector.sel_popUpStatusItemMenu_.value;
public static final long sel_port = Selector.sel_port.value;
public static final long sel_postEvent_atStart_ = Selector.sel_postEvent_atStart_.value;
public static final long sel_preparedCellAtColumn_row_ = Selector.sel_preparedCellAtColumn_row_.value;
public static final long sel_prependTransform_ = Selector.sel_prependTransform_.value;
public static final long sel_pressedMouseButtons = Selector.sel_pressedMouseButtons.value;
public static final long sel_preventDefault = Selector.sel_preventDefault.value;
public static final long sel_previousFailureCount = Selector.sel_previousFailureCount.value;
public static final long sel_printDocumentView = Selector.sel_printDocumentView.value;
public static final long sel_printOperationWithPrintInfo_ = Selector.sel_printOperationWithPrintInfo_.value;
public static final long sel_printOperationWithView_printInfo_ = Selector.sel_printOperationWithView_printInfo_.value;
public static final long sel_printPanel = Selector.sel_printPanel.value;
public static final long sel_printer = Selector.sel_printer.value;
public static final long sel_printerNames = Selector.sel_printerNames.value;
public static final long sel_printerWithName_ = Selector.sel_printerWithName_.value;
public static final long sel_processInfo = Selector.sel_processInfo.value;
public static final long sel_propertyListForType_ = Selector.sel_propertyListForType_.value;
public static final long sel_proposedCredential = Selector.sel_proposedCredential.value;
public static final long sel_protectionSpace = Selector.sel_protectionSpace.value;
public static final long sel_push = Selector.sel_push.value;
public static final long sel_rangeValue = Selector.sel_rangeValue.value;
public static final long sel_readSelectionFromPasteboard_ = Selector.sel_readSelectionFromPasteboard_.value;
public static final long sel_realm = Selector.sel_realm.value;
public static final long sel_recentSearches = Selector.sel_recentSearches.value;
public static final long sel_rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCount_ = Selector.sel_rectArrayForCharacterRange_withinSelectedCharacterRange_inTextContainer_rectCount_.value;
public static final long sel_rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCount_ = Selector.sel_rectArrayForGlyphRange_withinSelectedGlyphRange_inTextContainer_rectCount_.value;
public static final long sel_rectForPart_ = Selector.sel_rectForPart_.value;
public static final long sel_rectOfColumn_ = Selector.sel_rectOfColumn_.value;
public static final long sel_rectOfRow_ = Selector.sel_rectOfRow_.value;
public static final long sel_rectValue = Selector.sel_rectValue.value;
public static final long sel_redComponent = Selector.sel_redComponent.value;
public static final long sel_redo = Selector.sel_redo.value;
public static final long sel_reflectScrolledClipView_ = Selector.sel_reflectScrolledClipView_.value;
public static final long sel_registerForDraggedTypes_ = Selector.sel_registerForDraggedTypes_.value;
public static final long sel_release = Selector.sel_release.value;
public static final long sel_reload_ = Selector.sel_reload_.value;
public static final long sel_reloadData = Selector.sel_reloadData.value;
public static final long sel_reloadItem_reloadChildren_ = Selector.sel_reloadItem_reloadChildren_.value;
public static final long sel_removeAllItems = Selector.sel_removeAllItems.value;
public static final long sel_removeAllPoints = Selector.sel_removeAllPoints.value;
public static final long sel_removeAttribute_range_ = Selector.sel_removeAttribute_range_.value;
public static final long sel_removeChildWindow_ = Selector.sel_removeChildWindow_.value;
public static final long sel_removeColorWithKey_ = Selector.sel_removeColorWithKey_.value;
public static final long sel_removeFromSuperview = Selector.sel_removeFromSuperview.value;
public static final long sel_removeItem_ = Selector.sel_removeItem_.value;
public static final long sel_removeItemAtIndex_ = Selector.sel_removeItemAtIndex_.value;
public static final long sel_removeLastObject = Selector.sel_removeLastObject.value;
public static final long sel_removeObject_ = Selector.sel_removeObject_.value;
public static final long sel_removeObjectAtIndex_ = Selector.sel_removeObjectAtIndex_.value;
public static final long sel_removeObjectForKey_ = Selector.sel_removeObjectForKey_.value;
public static final long sel_removeObjectIdenticalTo_ = Selector.sel_removeObjectIdenticalTo_.value;
public static final long sel_removeObserver_ = Selector.sel_removeObserver_.value;
public static final long sel_removeRepresentation_ = Selector.sel_removeRepresentation_.value;
public static final long sel_removeStatusItem_ = Selector.sel_removeStatusItem_.value;
public static final long sel_removeTabViewItem_ = Selector.sel_removeTabViewItem_.value;
public static final long sel_removeTableColumn_ = Selector.sel_removeTableColumn_.value;
public static final long sel_removeTemporaryAttribute_forCharacterRange_ = Selector.sel_removeTemporaryAttribute_forCharacterRange_.value;
public static final long sel_removeToolTip_ = Selector.sel_removeToolTip_.value;
public static final long sel_removeTrackingArea_ = Selector.sel_removeTrackingArea_.value;
public static final long sel_replaceCharactersInRange_withString_ = Selector.sel_replaceCharactersInRange_withString_.value;
public static final long sel_replyToOpenOrPrint_ = Selector.sel_replyToOpenOrPrint_.value;
public static final long sel_representation = Selector.sel_representation.value;
public static final long sel_representations = Selector.sel_representations.value;
public static final long sel_request = Selector.sel_request.value;
public static final long sel_requestWithURL_ = Selector.sel_requestWithURL_.value;
public static final long sel_resetCursorRects = Selector.sel_resetCursorRects.value;
public static final long sel_resignFirstResponder = Selector.sel_resignFirstResponder.value;
public static final long sel_resizeDownCursor = Selector.sel_resizeDownCursor.value;
public static final long sel_resizeLeftCursor = Selector.sel_resizeLeftCursor.value;
public static final long sel_resizeLeftRightCursor = Selector.sel_resizeLeftRightCursor.value;
public static final long sel_resizeRightCursor = Selector.sel_resizeRightCursor.value;
public static final long sel_resizeUpCursor = Selector.sel_resizeUpCursor.value;
public static final long sel_resizeUpDownCursor = Selector.sel_resizeUpDownCursor.value;
public static final long sel_resizingMask = Selector.sel_resizingMask.value;
public static final long sel_respondsToSelector_ = Selector.sel_respondsToSelector_.value;
public static final long sel_restoreGraphicsState = Selector.sel_restoreGraphicsState.value;
public static final long sel_retain = Selector.sel_retain.value;
public static final long sel_retainCount = Selector.sel_retainCount.value;
public static final long sel_rightMouseDown_ = Selector.sel_rightMouseDown_.value;
public static final long sel_rightMouseDragged_ = Selector.sel_rightMouseDragged_.value;
public static final long sel_rightMouseUp_ = Selector.sel_rightMouseUp_.value;
public static final long sel_rotateByDegrees_ = Selector.sel_rotateByDegrees_.value;
public static final long sel_rotateWithEvent_ = Selector.sel_rotateWithEvent_.value;
public static final long sel_rotation = Selector.sel_rotation.value;
public static final long sel_rowAtPoint_ = Selector.sel_rowAtPoint_.value;
public static final long sel_rowForItem_ = Selector.sel_rowForItem_.value;
public static final long sel_rowHeight = Selector.sel_rowHeight.value;
public static final long sel_runModal = Selector.sel_runModal.value;
public static final long sel_runModalForDirectory_file_ = Selector.sel_runModalForDirectory_file_.value;
public static final long sel_runModalForWindow_ = Selector.sel_runModalForWindow_.value;
public static final long sel_runModalWithPrintInfo_ = Selector.sel_runModalWithPrintInfo_.value;
public static final long sel_runMode_beforeDate_ = Selector.sel_runMode_beforeDate_.value;
public static final long sel_runOperation = Selector.sel_runOperation.value;
public static final long sel_samplesPerPixel = Selector.sel_samplesPerPixel.value;
public static final long sel_saveGraphicsState = Selector.sel_saveGraphicsState.value;
public static final long sel_savePanel = Selector.sel_savePanel.value;
public static final long sel_scaleXBy_yBy_ = Selector.sel_scaleXBy_yBy_.value;
public static final long sel_scheduledTimerWithTimeInterval_target_selector_userInfo_repeats_ = Selector.sel_scheduledTimerWithTimeInterval_target_selector_userInfo_repeats_.value;
public static final long sel_screen = Selector.sel_screen.value;
public static final long sel_screenX = Selector.sel_screenX.value;
public static final long sel_screenY = Selector.sel_screenY.value;
public static final long sel_screens = Selector.sel_screens.value;
public static final long sel_scrollClipView_toPoint_ = Selector.sel_scrollClipView_toPoint_.value;
public static final long sel_scrollColumnToVisible_ = Selector.sel_scrollColumnToVisible_.value;
public static final long sel_scrollPoint_ = Selector.sel_scrollPoint_.value;
public static final long sel_scrollRangeToVisible_ = Selector.sel_scrollRangeToVisible_.value;
public static final long sel_scrollRect_by_ = Selector.sel_scrollRect_by_.value;
public static final long sel_scrollRowToVisible_ = Selector.sel_scrollRowToVisible_.value;
public static final long sel_scrollWheel_ = Selector.sel_scrollWheel_.value;
public static final long sel_scrollerStyle = Selector.sel_scrollerStyle.value;
public static final long sel_scrollerWidthForControlSize_ = Selector.sel_scrollerWidthForControlSize_.value;
public static final long sel_searchButtonCell = Selector.sel_searchButtonCell.value;
public static final long sel_searchButtonRectForBounds_ = Selector.sel_searchButtonRectForBounds_.value;
public static final long sel_searchTextRectForBounds_ = Selector.sel_searchTextRectForBounds_.value;
public static final long sel_secondOfMinute = Selector.sel_secondOfMinute.value;
public static final long sel_secondarySelectedControlColor = Selector.sel_secondarySelectedControlColor.value;
public static final long sel_selectAll_ = Selector.sel_selectAll_.value;
public static final long sel_selectItem_ = Selector.sel_selectItem_.value;
public static final long sel_selectItemAtIndex_ = Selector.sel_selectItemAtIndex_.value;
public static final long sel_selectRowIndexes_byExtendingSelection_ = Selector.sel_selectRowIndexes_byExtendingSelection_.value;
public static final long sel_selectTabViewItemAtIndex_ = Selector.sel_selectTabViewItemAtIndex_.value;
public static final long sel_selectText_ = Selector.sel_selectText_.value;
public static final long sel_selectedControlColor = Selector.sel_selectedControlColor.value;
public static final long sel_selectedControlTextColor = Selector.sel_selectedControlTextColor.value;
public static final long sel_selectedRange = Selector.sel_selectedRange.value;
public static final long sel_selectedRow = Selector.sel_selectedRow.value;
public static final long sel_selectedRowIndexes = Selector.sel_selectedRowIndexes.value;
public static final long sel_selectedTabViewItem = Selector.sel_selectedTabViewItem.value;
public static final long sel_selectedTextAttributes = Selector.sel_selectedTextAttributes.value;
public static final long sel_selectedTextBackgroundColor = Selector.sel_selectedTextBackgroundColor.value;
public static final long sel_selectedTextColor = Selector.sel_selectedTextColor.value;
public static final long sel_sendAction_to_ = Selector.sel_sendAction_to_.value;
public static final long sel_sendAction_to_from_ = Selector.sel_sendAction_to_from_.value;
public static final long sel_sendEvent_ = Selector.sel_sendEvent_.value;
public static final long sel_sender = Selector.sel_sender.value;
public static final long sel_separatorItem = Selector.sel_separatorItem.value;
public static final long sel_set = Selector.sel_set.value;
public static final long sel_setAcceptsMouseMovedEvents_ = Selector.sel_setAcceptsMouseMovedEvents_.value;
public static final long sel_setAcceptsTouchEvents_ = Selector.sel_setAcceptsTouchEvents_.value;
public static final long sel_setAccessoryView_ = Selector.sel_setAccessoryView_.value;
public static final long sel_setAction_ = Selector.sel_setAction_.value;
public static final long sel_setActivationPolicy_ = Selector.sel_setActivationPolicy_.value;
public static final long sel_setAlertStyle_ = Selector.sel_setAlertStyle_.value;
public static final long sel_setAlignment_ = Selector.sel_setAlignment_.value;
public static final long sel_setAllowedFileTypes_ = Selector.sel_setAllowedFileTypes_.value;
public static final long sel_setAllowsColumnReordering_ = Selector.sel_setAllowsColumnReordering_.value;
public static final long sel_setAllowsFloats_ = Selector.sel_setAllowsFloats_.value;
public static final long sel_setAllowsMixedState_ = Selector.sel_setAllowsMixedState_.value;
public static final long sel_setAllowsMultipleSelection_ = Selector.sel_setAllowsMultipleSelection_.value;
public static final long sel_setAllowsOtherFileTypes_ = Selector.sel_setAllowsOtherFileTypes_.value;
public static final long sel_setAllowsUndo_ = Selector.sel_setAllowsUndo_.value;
public static final long sel_setAllowsUserCustomization_ = Selector.sel_setAllowsUserCustomization_.value;
public static final long sel_setAlpha_ = Selector.sel_setAlpha_.value;
public static final long sel_setAlphaValue_ = Selector.sel_setAlphaValue_.value;
public static final long sel_setAlternateButtonTitle_ = Selector.sel_setAlternateButtonTitle_.value;
public static final long sel_setAppearance_ = Selector.sel_setAppearance_.value;
public static final long sel_setApplicationIconImage_ = Selector.sel_setApplicationIconImage_.value;
public static final long sel_setApplicationNameForUserAgent_ = Selector.sel_setApplicationNameForUserAgent_.value;
public static final long sel_setAttachmentCell_ = Selector.sel_setAttachmentCell_.value;
public static final long sel_setAttributedString_ = Selector.sel_setAttributedString_.value;
public static final long sel_setAttributedStringValue_ = Selector.sel_setAttributedStringValue_.value;
public static final long sel_setAttributedTitle_ = Selector.sel_setAttributedTitle_.value;
public static final long sel_setAutoenablesItems_ = Selector.sel_setAutoenablesItems_.value;
public static final long sel_setAutohidesScrollers_ = Selector.sel_setAutohidesScrollers_.value;
public static final long sel_setAutoresizesOutlineColumn_ = Selector.sel_setAutoresizesOutlineColumn_.value;
public static final long sel_setAutoresizesSubviews_ = Selector.sel_setAutoresizesSubviews_.value;
public static final long sel_setAutoresizingMask_ = Selector.sel_setAutoresizingMask_.value;
public static final long sel_setAutosaveExpandedItems_ = Selector.sel_setAutosaveExpandedItems_.value;
public static final long sel_setBackgroundColor_ = Selector.sel_setBackgroundColor_.value;
public static final long sel_setBackgroundLayoutEnabled_ = Selector.sel_setBackgroundLayoutEnabled_.value;
public static final long sel_setBackgroundStyle_ = Selector.sel_setBackgroundStyle_.value;
public static final long sel_setBadgeLabel_ = Selector.sel_setBadgeLabel_.value;
public static final long sel_setBaseWritingDirection_ = Selector.sel_setBaseWritingDirection_.value;
public static final long sel_setBecomesKeyOnlyIfNeeded_ = Selector.sel_setBecomesKeyOnlyIfNeeded_.value;
public static final long sel_setBezelStyle_ = Selector.sel_setBezelStyle_.value;
public static final long sel_setBezeled_ = Selector.sel_setBezeled_.value;
public static final long sel_setBorderType_ = Selector.sel_setBorderType_.value;
public static final long sel_setBorderWidth_ = Selector.sel_setBorderWidth_.value;
public static final long sel_setBordered_ = Selector.sel_setBordered_.value;
public static final long sel_setBoundsRotation_ = Selector.sel_setBoundsRotation_.value;
public static final long sel_setBoundsSize_ = Selector.sel_setBoundsSize_.value;
public static final long sel_setBoxType_ = Selector.sel_setBoxType_.value;
public static final long sel_setButtonType_ = Selector.sel_setButtonType_.value;
public static final long sel_setCacheMode_ = Selector.sel_setCacheMode_.value;
public static final long sel_setCachePolicy_ = Selector.sel_setCachePolicy_.value;
public static final long sel_setCanChooseDirectories_ = Selector.sel_setCanChooseDirectories_.value;
public static final long sel_setCanChooseFiles_ = Selector.sel_setCanChooseFiles_.value;
public static final long sel_setCanCreateDirectories_ = Selector.sel_setCanCreateDirectories_.value;
public static final long sel_setCancelButtonCell_ = Selector.sel_setCancelButtonCell_.value;
public static final long sel_setCell_ = Selector.sel_setCell_.value;
public static final long sel_setCellClass_ = Selector.sel_setCellClass_.value;
public static final long sel_setClip = Selector.sel_setClip.value;
public static final long sel_setCollectionBehavior_ = Selector.sel_setCollectionBehavior_.value;
public static final long sel_setColor_ = Selector.sel_setColor_.value;
public static final long sel_setColumnAutoresizingStyle_ = Selector.sel_setColumnAutoresizingStyle_.value;
public static final long sel_setCompositingOperation_ = Selector.sel_setCompositingOperation_.value;
public static final long sel_setContainerSize_ = Selector.sel_setContainerSize_.value;
public static final long sel_setContentView_ = Selector.sel_setContentView_.value;
public static final long sel_setContentViewMargins_ = Selector.sel_setContentViewMargins_.value;
public static final long sel_setControlSize_ = Selector.sel_setControlSize_.value;
public static final long sel_setCookie_ = Selector.sel_setCookie_.value;
public static final long sel_setCopiesOnScroll_ = Selector.sel_setCopiesOnScroll_.value;
public static final long sel_setCurrentContext_ = Selector.sel_setCurrentContext_.value;
public static final long sel_setCurrentOperation_ = Selector.sel_setCurrentOperation_.value;
public static final long sel_setCustomUserAgent_ = Selector.sel_setCustomUserAgent_.value;
public static final long sel_setData_forType_ = Selector.sel_setData_forType_.value;
public static final long sel_setDataCell_ = Selector.sel_setDataCell_.value;
public static final long sel_setDataSource_ = Selector.sel_setDataSource_.value;
public static final long sel_setDatePickerElements_ = Selector.sel_setDatePickerElements_.value;
public static final long sel_setDatePickerStyle_ = Selector.sel_setDatePickerStyle_.value;
public static final long sel_setDateValue_ = Selector.sel_setDateValue_.value;
public static final long sel_setDefaultButtonCell_ = Selector.sel_setDefaultButtonCell_.value;
public static final long sel_setDefaultFlatness_ = Selector.sel_setDefaultFlatness_.value;
public static final long sel_setDefaultParagraphStyle_ = Selector.sel_setDefaultParagraphStyle_.value;
public static final long sel_setDefaultTabInterval_ = Selector.sel_setDefaultTabInterval_.value;
public static final long sel_setDelegate_ = Selector.sel_setDelegate_.value;
public static final long sel_setDestination_allowOverwrite_ = Selector.sel_setDestination_allowOverwrite_.value;
public static final long sel_setDictionary_ = Selector.sel_setDictionary_.value;
public static final long sel_setDirectory_ = Selector.sel_setDirectory_.value;
public static final long sel_setDirectoryURL_ = Selector.sel_setDirectoryURL_.value;
public static final long sel_setDisplayMode_ = Selector.sel_setDisplayMode_.value;
public static final long sel_setDisplaysLinkToolTips_ = Selector.sel_setDisplaysLinkToolTips_.value;
public static final long sel_setDocumentCursor_ = Selector.sel_setDocumentCursor_.value;
public static final long sel_setDocumentEdited_ = Selector.sel_setDocumentEdited_.value;
public static final long sel_setDocumentView_ = Selector.sel_setDocumentView_.value;
public static final long sel_setDoubleAction_ = Selector.sel_setDoubleAction_.value;
public static final long sel_setDoubleValue_ = Selector.sel_setDoubleValue_.value;
public static final long sel_setDownloadDelegate_ = Selector.sel_setDownloadDelegate_.value;
public static final long sel_setDrawsBackground_ = Selector.sel_setDrawsBackground_.value;
public static final long sel_setDropItem_dropChildIndex_ = Selector.sel_setDropItem_dropChildIndex_.value;
public static final long sel_setDropRow_dropOperation_ = Selector.sel_setDropRow_dropOperation_.value;
public static final long sel_setEditable_ = Selector.sel_setEditable_.value;
public static final long sel_setEnabled_ = Selector.sel_setEnabled_.value;
public static final long sel_setFill = Selector.sel_setFill.value;
public static final long sel_setFillColor_ = Selector.sel_setFillColor_.value;
public static final long sel_setFireDate_ = Selector.sel_setFireDate_.value;
public static final long sel_setFirstLineHeadIndent_ = Selector.sel_setFirstLineHeadIndent_.value;
public static final long sel_setFloatingPanel_ = Selector.sel_setFloatingPanel_.value;
public static final long sel_setFocusRingType_ = Selector.sel_setFocusRingType_.value;
public static final long sel_setFont_ = Selector.sel_setFont_.value;
public static final long sel_setFormatter_ = Selector.sel_setFormatter_.value;
public static final long sel_setFrame_ = Selector.sel_setFrame_.value;
public static final long sel_setFrame_display_ = Selector.sel_setFrame_display_.value;
public static final long sel_setFrame_display_animate_ = Selector.sel_setFrame_display_animate_.value;
public static final long sel_setFrameFromContentFrame_ = Selector.sel_setFrameFromContentFrame_.value;
public static final long sel_setFrameLoadDelegate_ = Selector.sel_setFrameLoadDelegate_.value;
public static final long sel_setFrameOrigin_ = Selector.sel_setFrameOrigin_.value;
public static final long sel_setFrameSize_ = Selector.sel_setFrameSize_.value;
public static final long sel_setGridStyleMask_ = Selector.sel_setGridStyleMask_.value;
public static final long sel_setHTTPBody_ = Selector.sel_setHTTPBody_.value;
public static final long sel_setHTTPMethod_ = Selector.sel_setHTTPMethod_.value;
public static final long sel_setHasHorizontalScroller_ = Selector.sel_setHasHorizontalScroller_.value;
public static final long sel_setHasShadow_ = Selector.sel_setHasShadow_.value;
public static final long sel_setHasVerticalScroller_ = Selector.sel_setHasVerticalScroller_.value;
public static final long sel_setHeadIndent_ = Selector.sel_setHeadIndent_.value;
public static final long sel_setHeaderCell_ = Selector.sel_setHeaderCell_.value;
public static final long sel_setHeaderView_ = Selector.sel_setHeaderView_.value;
public static final long sel_setHelpMenu_ = Selector.sel_setHelpMenu_.value;
public static final long sel_setHidden_ = Selector.sel_setHidden_.value;
public static final long sel_setHiddenUntilMouseMoves_ = Selector.sel_setHiddenUntilMouseMoves_.value;
public static final long sel_setHidesOnDeactivate_ = Selector.sel_setHidesOnDeactivate_.value;
public static final long sel_setHighlightMode_ = Selector.sel_setHighlightMode_.value;
public static final long sel_setHighlighted_ = Selector.sel_setHighlighted_.value;
public static final long sel_setHighlightedTableColumn_ = Selector.sel_setHighlightedTableColumn_.value;
public static final long sel_setHighlightsBy_ = Selector.sel_setHighlightsBy_.value;
public static final long sel_setHorizontalScroller_ = Selector.sel_setHorizontalScroller_.value;
public static final long sel_setHorizontallyResizable_ = Selector.sel_setHorizontallyResizable_.value;
public static final long sel_setIcon_ = Selector.sel_setIcon_.value;
public static final long sel_setIdentifier_ = Selector.sel_setIdentifier_.value;
public static final long sel_setImage_ = Selector.sel_setImage_.value;
public static final long sel_setImageAlignment_ = Selector.sel_setImageAlignment_.value;
public static final long sel_setImageInterpolation_ = Selector.sel_setImageInterpolation_.value;
public static final long sel_setImagePosition_ = Selector.sel_setImagePosition_.value;
public static final long sel_setImageScaling_ = Selector.sel_setImageScaling_.value;
public static final long sel_setIncrement_ = Selector.sel_setIncrement_.value;
public static final long sel_setIndeterminate_ = Selector.sel_setIndeterminate_.value;
public static final long sel_setIndicatorImage_inTableColumn_ = Selector.sel_setIndicatorImage_inTableColumn_.value;
public static final long sel_setInteger_forKey_ = Selector.sel_setInteger_forKey_.value;
public static final long sel_setIntercellSpacing_ = Selector.sel_setIntercellSpacing_.value;
public static final long sel_setJavaEnabled_ = Selector.sel_setJavaEnabled_.value;
public static final long sel_setJavaScriptEnabled_ = Selector.sel_setJavaScriptEnabled_.value;
public static final long sel_setJobDisposition_ = Selector.sel_setJobDisposition_.value;
public static final long sel_setJobTitle_ = Selector.sel_setJobTitle_.value;
public static final long sel_setKeyEquivalent_ = Selector.sel_setKeyEquivalent_.value;
public static final long sel_setKeyEquivalentModifierMask_ = Selector.sel_setKeyEquivalentModifierMask_.value;
public static final long sel_setKnobProportion_ = Selector.sel_setKnobProportion_.value;
public static final long sel_setLabel_ = Selector.sel_setLabel_.value;
public static final long sel_setLength_ = Selector.sel_setLength_.value;
public static final long sel_setLevel_ = Selector.sel_setLevel_.value;
public static final long sel_setLineBreakMode_ = Selector.sel_setLineBreakMode_.value;
public static final long sel_setLineCapStyle_ = Selector.sel_setLineCapStyle_.value;
public static final long sel_setLineDash_count_phase_ = Selector.sel_setLineDash_count_phase_.value;
public static final long sel_setLineFragmentPadding_ = Selector.sel_setLineFragmentPadding_.value;
public static final long sel_setLineJoinStyle_ = Selector.sel_setLineJoinStyle_.value;
public static final long sel_setLineSpacing_ = Selector.sel_setLineSpacing_.value;
public static final long sel_setLineWidth_ = Selector.sel_setLineWidth_.value;
public static final long sel_setLinkTextAttributes_ = Selector.sel_setLinkTextAttributes_.value;
public static final long sel_setMainMenu_ = Selector.sel_setMainMenu_.value;
public static final long sel_setMarkedText_selectedRange_ = Selector.sel_setMarkedText_selectedRange_.value;
public static final long sel_setMaxSize_ = Selector.sel_setMaxSize_.value;
public static final long sel_setMaxValue_ = Selector.sel_setMaxValue_.value;
public static final long sel_setMaximum_ = Selector.sel_setMaximum_.value;
public static final long sel_setMaximumFractionDigits_ = Selector.sel_setMaximumFractionDigits_.value;
public static final long sel_setMaximumIntegerDigits_ = Selector.sel_setMaximumIntegerDigits_.value;
public static final long sel_setMenu_ = Selector.sel_setMenu_.value;
public static final long sel_setMenuFormRepresentation_ = Selector.sel_setMenuFormRepresentation_.value;
public static final long sel_setMessage_ = Selector.sel_setMessage_.value;
public static final long sel_setMessageText_ = Selector.sel_setMessageText_.value;
public static final long sel_setMinSize_ = Selector.sel_setMinSize_.value;
public static final long sel_setMinValue_ = Selector.sel_setMinValue_.value;
public static final long sel_setMinWidth_ = Selector.sel_setMinWidth_.value;
public static final long sel_setMinimum_ = Selector.sel_setMinimum_.value;
public static final long sel_setMinimumFractionDigits_ = Selector.sel_setMinimumFractionDigits_.value;
public static final long sel_setMinimumIntegerDigits_ = Selector.sel_setMinimumIntegerDigits_.value;
public static final long sel_setMiterLimit_ = Selector.sel_setMiterLimit_.value;
public static final long sel_setMovable_ = Selector.sel_setMovable_.value;
public static final long sel_setNameFieldStringValue_ = Selector.sel_setNameFieldStringValue_.value;
public static final long sel_setNeedsDisplay_ = Selector.sel_setNeedsDisplay_.value;
public static final long sel_setNeedsDisplayInRect_ = Selector.sel_setNeedsDisplayInRect_.value;
public static final long sel_setNumberOfVisibleItems_ = Selector.sel_setNumberOfVisibleItems_.value;
public static final long sel_setNumberStyle_ = Selector.sel_setNumberStyle_.value;
public static final long sel_setObject_forKey_ = Selector.sel_setObject_forKey_.value;
public static final long sel_setObjectValue_ = Selector.sel_setObjectValue_.value;
public static final long sel_setOnMouseEntered_ = Selector.sel_setOnMouseEntered_.value;
public static final long sel_setOpaque_ = Selector.sel_setOpaque_.value;
public static final long sel_setOptions_ = Selector.sel_setOptions_.value;
public static final long sel_setOutlineTableColumn_ = Selector.sel_setOutlineTableColumn_.value;
public static final long sel_setPaletteLabel_ = Selector.sel_setPaletteLabel_.value;
public static final long sel_setPanelFont_isMultiple_ = Selector.sel_setPanelFont_isMultiple_.value;
public static final long sel_setPartialStringValidationEnabled_ = Selector.sel_setPartialStringValidationEnabled_.value;
public static final long sel_setPatternPhase_ = Selector.sel_setPatternPhase_.value;
public static final long sel_setPlaceholderString_ = Selector.sel_setPlaceholderString_.value;
public static final long sel_setPolicyDelegate_ = Selector.sel_setPolicyDelegate_.value;
public static final long sel_setPreferences_ = Selector.sel_setPreferences_.value;
public static final long sel_setPrinter_ = Selector.sel_setPrinter_.value;
public static final long sel_setPropertyList_forType_ = Selector.sel_setPropertyList_forType_.value;
public static final long sel_setPullsDown_ = Selector.sel_setPullsDown_.value;
public static final long sel_setReleasedWhenClosed_ = Selector.sel_setReleasedWhenClosed_.value;
public static final long sel_setRepresentedFilename_ = Selector.sel_setRepresentedFilename_.value;
public static final long sel_setRepresentedURL_ = Selector.sel_setRepresentedURL_.value;
public static final long sel_setResizingMask_ = Selector.sel_setResizingMask_.value;
public static final long sel_setResourceLoadDelegate_ = Selector.sel_setResourceLoadDelegate_.value;
public static final long sel_setRichText_ = Selector.sel_setRichText_.value;
public static final long sel_setRowHeight_ = Selector.sel_setRowHeight_.value;
public static final long sel_setScalesWhenResized_ = Selector.sel_setScalesWhenResized_.value;
public static final long sel_setScrollable_ = Selector.sel_setScrollable_.value;
public static final long sel_setSearchButtonCell_ = Selector.sel_setSearchButtonCell_.value;
public static final long sel_setSelectable_ = Selector.sel_setSelectable_.value;
public static final long sel_setSelectedItemIdentifier_ = Selector.sel_setSelectedItemIdentifier_.value;
public static final long sel_setSelectedRange_ = Selector.sel_setSelectedRange_.value;
public static final long sel_setSelectedTextAttributes_ = Selector.sel_setSelectedTextAttributes_.value;
public static final long sel_setSelectionOnly_ = Selector.sel_setSelectionOnly_.value;
public static final long sel_setServicesMenu_ = Selector.sel_setServicesMenu_.value;
public static final long sel_setShouldAntialias_ = Selector.sel_setShouldAntialias_.value;
public static final long sel_setShowsHelp_ = Selector.sel_setShowsHelp_.value;
public static final long sel_setShowsPrintPanel_ = Selector.sel_setShowsPrintPanel_.value;
public static final long sel_setShowsProgressPanel_ = Selector.sel_setShowsProgressPanel_.value;
public static final long sel_setShowsResizeIndicator_ = Selector.sel_setShowsResizeIndicator_.value;
public static final long sel_setShowsToolbarButton_ = Selector.sel_setShowsToolbarButton_.value;
public static final long sel_setSize_ = Selector.sel_setSize_.value;
public static final long sel_setState_ = Selector.sel_setState_.value;
public static final long sel_setString_ = Selector.sel_setString_.value;
public static final long sel_setString_forType_ = Selector.sel_setString_forType_.value;
public static final long sel_setStringValue_ = Selector.sel_setStringValue_.value;
public static final long sel_setStroke = Selector.sel_setStroke.value;
public static final long sel_setSubmenu_ = Selector.sel_setSubmenu_.value;
public static final long sel_setSubmenu_forItem_ = Selector.sel_setSubmenu_forItem_.value;
public static final long sel_setTabStops_ = Selector.sel_setTabStops_.value;
public static final long sel_setTabViewType_ = Selector.sel_setTabViewType_.value;
public static final long sel_setTag_ = Selector.sel_setTag_.value;
public static final long sel_setTarget_ = Selector.sel_setTarget_.value;
public static final long sel_setTextColor_ = Selector.sel_setTextColor_.value;
public static final long sel_setTextStorage_ = Selector.sel_setTextStorage_.value;
public static final long sel_setTitle_ = Selector.sel_setTitle_.value;
public static final long sel_setTitleFont_ = Selector.sel_setTitleFont_.value;
public static final long sel_setTitlePosition_ = Selector.sel_setTitlePosition_.value;
public static final long sel_setToolTip_ = Selector.sel_setToolTip_.value;
public static final long sel_setToolbar_ = Selector.sel_setToolbar_.value;
public static final long sel_setTransformStruct_ = Selector.sel_setTransformStruct_.value;
public static final long sel_setTreatsFilePackagesAsDirectories_ = Selector.sel_setTreatsFilePackagesAsDirectories_.value;
public static final long sel_setUIDelegate_ = Selector.sel_setUIDelegate_.value;
public static final long sel_setURL_ = Selector.sel_setURL_.value;
public static final long sel_setUpPrintOperationDefaultValues = Selector.sel_setUpPrintOperationDefaultValues.value;
public static final long sel_setUsesAlternatingRowBackgroundColors_ = Selector.sel_setUsesAlternatingRowBackgroundColors_.value;
public static final long sel_setUsesFontPanel_ = Selector.sel_setUsesFontPanel_.value;
public static final long sel_setUsesScreenFonts_ = Selector.sel_setUsesScreenFonts_.value;
public static final long sel_setUsesSingleLineMode_ = Selector.sel_setUsesSingleLineMode_.value;
public static final long sel_setUsesThreadedAnimation_ = Selector.sel_setUsesThreadedAnimation_.value;
public static final long sel_setValue_forHTTPHeaderField_ = Selector.sel_setValue_forHTTPHeaderField_.value;
public static final long sel_setValue_forKey_ = Selector.sel_setValue_forKey_.value;
public static final long sel_setValueWraps_ = Selector.sel_setValueWraps_.value;
public static final long sel_setValues_forParameter_ = Selector.sel_setValues_forParameter_.value;
public static final long sel_setVerticalScrollElasticity_ = Selector.sel_setVerticalScrollElasticity_.value;
public static final long sel_setVerticalScroller_ = Selector.sel_setVerticalScroller_.value;
public static final long sel_setView_ = Selector.sel_setView_.value;
public static final long sel_setVisible_ = Selector.sel_setVisible_.value;
public static final long sel_setWantsRestingTouches_ = Selector.sel_setWantsRestingTouches_.value;
public static final long sel_setWidth_ = Selector.sel_setWidth_.value;
public static final long sel_setWidthTracksTextView_ = Selector.sel_setWidthTracksTextView_.value;
public static final long sel_setWindingRule_ = Selector.sel_setWindingRule_.value;
public static final long sel_setWorksWhenModal_ = Selector.sel_setWorksWhenModal_.value;
public static final long sel_setWraps_ = Selector.sel_setWraps_.value;
public static final long sel_sharedApplication = Selector.sel_sharedApplication.value;
public static final long sel_sharedCertificateTrustPanel = Selector.sel_sharedCertificateTrustPanel.value;
public static final long sel_sharedColorPanel = Selector.sel_sharedColorPanel.value;
public static final long sel_sharedFontManager = Selector.sel_sharedFontManager.value;
public static final long sel_sharedFontPanel = Selector.sel_sharedFontPanel.value;
public static final long sel_sharedHTTPCookieStorage = Selector.sel_sharedHTTPCookieStorage.value;
public static final long sel_sharedPrintInfo = Selector.sel_sharedPrintInfo.value;
public static final long sel_sharedWorkspace = Selector.sel_sharedWorkspace.value;
public static final long sel_shiftKey = Selector.sel_shiftKey.value;
public static final long sel_shouldAntialias = Selector.sel_shouldAntialias.value;
public static final long sel_shouldChangeTextInRange_replacementString_ = Selector.sel_shouldChangeTextInRange_replacementString_.value;
public static final long sel_shouldDelayWindowOrderingForEvent_ = Selector.sel_shouldDelayWindowOrderingForEvent_.value;
public static final long sel_shouldDrawInsertionPoint = Selector.sel_shouldDrawInsertionPoint.value;
public static final long sel_size = Selector.sel_size.value;
public static final long sel_sizeOfLabel_ = Selector.sel_sizeOfLabel_.value;
public static final long sel_sizeToFit = Selector.sel_sizeToFit.value;
public static final long sel_sizeValue = Selector.sel_sizeValue.value;
public static final long sel_skipDescendents = Selector.sel_skipDescendents.value;
public static final long sel_smallSystemFontSize = Selector.sel_smallSystemFontSize.value;
public static final long sel_sortIndicatorRectForBounds_ = Selector.sel_sortIndicatorRectForBounds_.value;
public static final long sel_standardPreferences = Selector.sel_standardPreferences.value;
public static final long sel_standardUserDefaults = Selector.sel_standardUserDefaults.value;
public static final long sel_startAnimation_ = Selector.sel_startAnimation_.value;
public static final long sel_state = Selector.sel_state.value;
public static final long sel_statusItemWithLength_ = Selector.sel_statusItemWithLength_.value;
public static final long sel_stop_ = Selector.sel_stop_.value;
public static final long sel_stopLoading_ = Selector.sel_stopLoading_.value;
public static final long sel_stopModal = Selector.sel_stopModal.value;
public static final long sel_string = Selector.sel_string.value;
public static final long sel_stringByAddingPercentEscapesUsingEncoding_ = Selector.sel_stringByAddingPercentEscapesUsingEncoding_.value;
public static final long sel_stringByAppendingPathComponent_ = Selector.sel_stringByAppendingPathComponent_.value;
public static final long sel_stringByAppendingPathExtension_ = Selector.sel_stringByAppendingPathExtension_.value;
public static final long sel_stringByAppendingString_ = Selector.sel_stringByAppendingString_.value;
public static final long sel_stringByDeletingLastPathComponent = Selector.sel_stringByDeletingLastPathComponent.value;
public static final long sel_stringByDeletingPathExtension = Selector.sel_stringByDeletingPathExtension.value;
public static final long sel_stringByReplacingOccurrencesOfString_withString_ = Selector.sel_stringByReplacingOccurrencesOfString_withString_.value;
public static final long sel_stringByReplacingPercentEscapesUsingEncoding_ = Selector.sel_stringByReplacingPercentEscapesUsingEncoding_.value;
public static final long sel_stringForKey_ = Selector.sel_stringForKey_.value;
public static final long sel_stringForObjectValue_ = Selector.sel_stringForObjectValue_.value;
public static final long sel_stringForType_ = Selector.sel_stringForType_.value;
public static final long sel_stringValue = Selector.sel_stringValue.value;
public static final long sel_stringWithCharacters_length_ = Selector.sel_stringWithCharacters_length_.value;
public static final long sel_stringWithUTF8String_ = Selector.sel_stringWithUTF8String_.value;
public static final long sel_stroke = Selector.sel_stroke.value;
public static final long sel_strokeRect_ = Selector.sel_strokeRect_.value;
public static final long sel_styleMask = Selector.sel_styleMask.value;
public static final long sel_submenu = Selector.sel_submenu.value;
public static final long sel_subviews = Selector.sel_subviews.value;
public static final long sel_superclass = Selector.sel_superclass.value;
public static final long sel_superview = Selector.sel_superview.value;
public static final long sel_swipeWithEvent_ = Selector.sel_swipeWithEvent_.value;
public static final long sel_systemFontOfSize_ = Selector.sel_systemFontOfSize_.value;
public static final long sel_systemFontSize = Selector.sel_systemFontSize.value;
public static final long sel_systemFontSizeForControlSize_ = Selector.sel_systemFontSizeForControlSize_.value;
public static final long sel_systemStatusBar = Selector.sel_systemStatusBar.value;
public static final long sel_systemVersion = Selector.sel_systemVersion.value;
public static final long sel_tabView_didSelectTabViewItem_ = Selector.sel_tabView_didSelectTabViewItem_.value;
public static final long sel_tabView_willSelectTabViewItem_ = Selector.sel_tabView_willSelectTabViewItem_.value;
public static final long sel_tabViewItemAtPoint_ = Selector.sel_tabViewItemAtPoint_.value;
public static final long sel_tableColumns = Selector.sel_tableColumns.value;
public static final long sel_tableView_acceptDrop_row_dropOperation_ = Selector.sel_tableView_acceptDrop_row_dropOperation_.value;
public static final long sel_tableView_didClickTableColumn_ = Selector.sel_tableView_didClickTableColumn_.value;
public static final long sel_tableView_objectValueForTableColumn_row_ = Selector.sel_tableView_objectValueForTableColumn_row_.value;
public static final long sel_tableView_setObjectValue_forTableColumn_row_ = Selector.sel_tableView_setObjectValue_forTableColumn_row_.value;
public static final long sel_tableView_shouldEditTableColumn_row_ = Selector.sel_tableView_shouldEditTableColumn_row_.value;
public static final long sel_tableView_shouldReorderColumn_toColumn_ = Selector.sel_tableView_shouldReorderColumn_toColumn_.value;
public static final long sel_tableView_shouldTrackCell_forTableColumn_row_ = Selector.sel_tableView_shouldTrackCell_forTableColumn_row_.value;
public static final long sel_tableView_validateDrop_proposedRow_proposedDropOperation_ = Selector.sel_tableView_validateDrop_proposedRow_proposedDropOperation_.value;
public static final long sel_tableView_willDisplayCell_forTableColumn_row_ = Selector.sel_tableView_willDisplayCell_forTableColumn_row_.value;
public static final long sel_tableView_writeRowsWithIndexes_toPasteboard_ = Selector.sel_tableView_writeRowsWithIndexes_toPasteboard_.value;
public static final long sel_tableViewColumnDidMove_ = Selector.sel_tableViewColumnDidMove_.value;
public static final long sel_tableViewColumnDidResize_ = Selector.sel_tableViewColumnDidResize_.value;
public static final long sel_tableViewSelectionDidChange_ = Selector.sel_tableViewSelectionDidChange_.value;
public static final long sel_tableViewSelectionIsChanging_ = Selector.sel_tableViewSelectionIsChanging_.value;
public static final long sel_tag = Selector.sel_tag.value;
public static final long sel_target = Selector.sel_target.value;
public static final long sel_terminate_ = Selector.sel_terminate_.value;
public static final long sel_testPart_ = Selector.sel_testPart_.value;
public static final long sel_textBackgroundColor = Selector.sel_textBackgroundColor.value;
public static final long sel_textColor = Selector.sel_textColor.value;
public static final long sel_textContainer = Selector.sel_textContainer.value;
public static final long sel_textDidChange_ = Selector.sel_textDidChange_.value;
public static final long sel_textDidEndEditing_ = Selector.sel_textDidEndEditing_.value;
public static final long sel_textStorage = Selector.sel_textStorage.value;
public static final long sel_textView_clickedOnLink_atIndex_ = Selector.sel_textView_clickedOnLink_atIndex_.value;
public static final long sel_textView_willChangeSelectionFromCharacterRange_toCharacterRange_ = Selector.sel_textView_willChangeSelectionFromCharacterRange_toCharacterRange_.value;
public static final long sel_textViewDidChangeSelection_ = Selector.sel_textViewDidChangeSelection_.value;
public static final long sel_thickness = Selector.sel_thickness.value;
public static final long sel_threadDictionary = Selector.sel_threadDictionary.value;
public static final long sel_tile = Selector.sel_tile.value;
public static final long sel_timeZone = Selector.sel_timeZone.value;
public static final long sel_timestamp = Selector.sel_timestamp.value;
public static final long sel_title = Selector.sel_title.value;
public static final long sel_titleCell = Selector.sel_titleCell.value;
public static final long sel_titleFont = Selector.sel_titleFont.value;
public static final long sel_titleOfSelectedItem = Selector.sel_titleOfSelectedItem.value;
public static final long sel_titleRectForBounds_ = Selector.sel_titleRectForBounds_.value;
public static final long sel_toggleFullScreen_ = Selector.sel_toggleFullScreen_.value;
public static final long sel_toolbar = Selector.sel_toolbar.value;
public static final long sel_toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar_ = Selector.sel_toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar_.value;
public static final long sel_toolbarAllowedItemIdentifiers_ = Selector.sel_toolbarAllowedItemIdentifiers_.value;
public static final long sel_toolbarDefaultItemIdentifiers_ = Selector.sel_toolbarDefaultItemIdentifiers_.value;
public static final long sel_toolbarSelectableItemIdentifiers_ = Selector.sel_toolbarSelectableItemIdentifiers_.value;
public static final long sel_touchesBeganWithEvent_ = Selector.sel_touchesBeganWithEvent_.value;
public static final long sel_touchesCancelledWithEvent_ = Selector.sel_touchesCancelledWithEvent_.value;
public static final long sel_touchesEndedWithEvent_ = Selector.sel_touchesEndedWithEvent_.value;
public static final long sel_touchesMatchingPhase_inView_ = Selector.sel_touchesMatchingPhase_inView_.value;
public static final long sel_touchesMovedWithEvent_ = Selector.sel_touchesMovedWithEvent_.value;
public static final long sel_trackingAreas = Selector.sel_trackingAreas.value;
public static final long sel_traitsOfFont_ = Selector.sel_traitsOfFont_.value;
public static final long sel_transform = Selector.sel_transform.value;
public static final long sel_transformPoint_ = Selector.sel_transformPoint_.value;
public static final long sel_transformSize_ = Selector.sel_transformSize_.value;
public static final long sel_transformStruct = Selector.sel_transformStruct.value;
public static final long sel_transformUsingAffineTransform_ = Selector.sel_transformUsingAffineTransform_.value;
public static final long sel_translateXBy_yBy_ = Selector.sel_translateXBy_yBy_.value;
public static final long sel_type = Selector.sel_type.value;
public static final long sel_type_conformsToType_ = Selector.sel_type_conformsToType_.value;
public static final long sel_typeOfFile_error_ = Selector.sel_typeOfFile_error_.value;
public static final long sel_types = Selector.sel_types.value;
public static final long sel_typesetter = Selector.sel_typesetter.value;
public static final long sel_unarchiveObjectWithData_ = Selector.sel_unarchiveObjectWithData_.value;
public static final long sel_undefined = Selector.sel_undefined.value;
public static final long sel_undo = Selector.sel_undo.value;
public static final long sel_undoManager = Selector.sel_undoManager.value;
public static final long sel_unhideAllApplications_ = Selector.sel_unhideAllApplications_.value;
public static final long sel_unlockFocus = Selector.sel_unlockFocus.value;
public static final long sel_unmarkText = Selector.sel_unmarkText.value;
public static final long sel_unregisterDraggedTypes = Selector.sel_unregisterDraggedTypes.value;
public static final long sel_update = Selector.sel_update.value;
public static final long sel_updateFromPMPrintSettings = Selector.sel_updateFromPMPrintSettings.value;
public static final long sel_updateTrackingAreas = Selector.sel_updateTrackingAreas.value;
public static final long sel_use = Selector.sel_use.value;
public static final long sel_useCredential_forAuthenticationChallenge_ = Selector.sel_useCredential_forAuthenticationChallenge_.value;
public static final long sel_usedRectForTextContainer_ = Selector.sel_usedRectForTextContainer_.value;
public static final long sel_user = Selector.sel_user.value;
public static final long sel_userInfo = Selector.sel_userInfo.value;
public static final long sel_usesAlternatingRowBackgroundColors = Selector.sel_usesAlternatingRowBackgroundColors.value;
public static final long sel_validAttributesForMarkedText = Selector.sel_validAttributesForMarkedText.value;
public static final long sel_validModesForFontPanel_ = Selector.sel_validModesForFontPanel_.value;
public static final long sel_validRequestorForSendType_returnType_ = Selector.sel_validRequestorForSendType_returnType_.value;
public static final long sel_validateMenuItem_ = Selector.sel_validateMenuItem_.value;
public static final long sel_validateVisibleColumns = Selector.sel_validateVisibleColumns.value;
public static final long sel_value = Selector.sel_value.value;
public static final long sel_valueForKey_ = Selector.sel_valueForKey_.value;
public static final long sel_valueWithPoint_ = Selector.sel_valueWithPoint_.value;
public static final long sel_valueWithRange_ = Selector.sel_valueWithRange_.value;
public static final long sel_valueWithRect_ = Selector.sel_valueWithRect_.value;
public static final long sel_valueWithSize_ = Selector.sel_valueWithSize_.value;
public static final long sel_view = Selector.sel_view.value;
public static final long sel_view_stringForToolTip_point_userData_ = Selector.sel_view_stringForToolTip_point_userData_.value;
public static final long sel_viewDidMoveToWindow = Selector.sel_viewDidMoveToWindow.value;
public static final long sel_viewWillMoveToWindow_ = Selector.sel_viewWillMoveToWindow_.value;
public static final long sel_visibleFrame = Selector.sel_visibleFrame.value;
public static final long sel_visibleRect = Selector.sel_visibleRect.value;
public static final long sel_wantsPeriodicDraggingUpdates = Selector.sel_wantsPeriodicDraggingUpdates.value;
public static final long sel_wantsToHandleMouseEvents = Selector.sel_wantsToHandleMouseEvents.value;
public static final long sel_webFrame = Selector.sel_webFrame.value;
public static final long sel_webScriptValueAtIndex_ = Selector.sel_webScriptValueAtIndex_.value;
public static final long sel_webView_contextMenuItemsForElement_defaultMenuItems_ = Selector.sel_webView_contextMenuItemsForElement_defaultMenuItems_.value;
public static final long sel_webView_createWebViewWithRequest_ = Selector.sel_webView_createWebViewWithRequest_.value;
public static final long sel_webView_decidePolicyForMIMEType_request_frame_decisionListener_ = Selector.sel_webView_decidePolicyForMIMEType_request_frame_decisionListener_.value;
public static final long sel_webView_decidePolicyForNavigationAction_request_frame_decisionListener_ = Selector.sel_webView_decidePolicyForNavigationAction_request_frame_decisionListener_.value;
public static final long sel_webView_decidePolicyForNewWindowAction_request_newFrameName_decisionListener_ = Selector.sel_webView_decidePolicyForNewWindowAction_request_newFrameName_decisionListener_.value;
public static final long sel_webView_didChangeLocationWithinPageForFrame_ = Selector.sel_webView_didChangeLocationWithinPageForFrame_.value;
public static final long sel_webView_didCommitLoadForFrame_ = Selector.sel_webView_didCommitLoadForFrame_.value;
public static final long sel_webView_didFailProvisionalLoadWithError_forFrame_ = Selector.sel_webView_didFailProvisionalLoadWithError_forFrame_.value;
public static final long sel_webView_didFinishLoadForFrame_ = Selector.sel_webView_didFinishLoadForFrame_.value;
public static final long sel_webView_didReceiveTitle_forFrame_ = Selector.sel_webView_didReceiveTitle_forFrame_.value;
public static final long sel_webView_didStartProvisionalLoadForFrame_ = Selector.sel_webView_didStartProvisionalLoadForFrame_.value;
public static final long sel_webView_identifierForInitialRequest_fromDataSource_ = Selector.sel_webView_identifierForInitialRequest_fromDataSource_.value;
public static final long sel_webView_mouseDidMoveOverElement_modifierFlags_ = Selector.sel_webView_mouseDidMoveOverElement_modifierFlags_.value;
public static final long sel_webView_printFrameView_ = Selector.sel_webView_printFrameView_.value;
public static final long sel_webView_resource_didFailLoadingWithError_fromDataSource_ = Selector.sel_webView_resource_didFailLoadingWithError_fromDataSource_.value;
public static final long sel_webView_resource_didFinishLoadingFromDataSource_ = Selector.sel_webView_resource_didFinishLoadingFromDataSource_.value;
public static final long sel_webView_resource_didReceiveAuthenticationChallenge_fromDataSource_ = Selector.sel_webView_resource_didReceiveAuthenticationChallenge_fromDataSource_.value;
public static final long sel_webView_resource_willSendRequest_redirectResponse_fromDataSource_ = Selector.sel_webView_resource_willSendRequest_redirectResponse_fromDataSource_.value;
public static final long sel_webView_runBeforeUnloadConfirmPanelWithMessage_initiatedByFrame_ = Selector.sel_webView_runBeforeUnloadConfirmPanelWithMessage_initiatedByFrame_.value;
public static final long sel_webView_runJavaScriptAlertPanelWithMessage_ = Selector.sel_webView_runJavaScriptAlertPanelWithMessage_.value;
public static final long sel_webView_runJavaScriptAlertPanelWithMessage_initiatedByFrame_ = Selector.sel_webView_runJavaScriptAlertPanelWithMessage_initiatedByFrame_.value;
public static final long sel_webView_runJavaScriptConfirmPanelWithMessage_ = Selector.sel_webView_runJavaScriptConfirmPanelWithMessage_.value;
public static final long sel_webView_runJavaScriptConfirmPanelWithMessage_initiatedByFrame_ = Selector.sel_webView_runJavaScriptConfirmPanelWithMessage_initiatedByFrame_.value;
public static final long sel_webView_runOpenPanelForFileButtonWithResultListener_ = Selector.sel_webView_runOpenPanelForFileButtonWithResultListener_.value;
public static final long sel_webView_setFrame_ = Selector.sel_webView_setFrame_.value;
public static final long sel_webView_setResizable_ = Selector.sel_webView_setResizable_.value;
public static final long sel_webView_setStatusBarVisible_ = Selector.sel_webView_setStatusBarVisible_.value;
public static final long sel_webView_setStatusText_ = Selector.sel_webView_setStatusText_.value;
public static final long sel_webView_setToolbarsVisible_ = Selector.sel_webView_setToolbarsVisible_.value;
public static final long sel_webView_unableToImplementPolicyWithError_frame_ = Selector.sel_webView_unableToImplementPolicyWithError_frame_.value;
public static final long sel_webView_windowScriptObjectAvailable_ = Selector.sel_webView_windowScriptObjectAvailable_.value;
public static final long sel_webViewClose_ = Selector.sel_webViewClose_.value;
public static final long sel_webViewFocus_ = Selector.sel_webViewFocus_.value;
public static final long sel_webViewShow_ = Selector.sel_webViewShow_.value;
public static final long sel_webViewUnfocus_ = Selector.sel_webViewUnfocus_.value;
public static final long sel_wheelDelta = Selector.sel_wheelDelta.value;
public static final long sel_wheelDeltaX = Selector.sel_wheelDeltaX.value;
public static final long sel_wheelDeltaY = Selector.sel_wheelDeltaY.value;
public static final long sel_width = Selector.sel_width.value;
public static final long sel_window = Selector.sel_window.value;
public static final long sel_windowBackgroundColor = Selector.sel_windowBackgroundColor.value;
public static final long sel_windowDidBecomeKey_ = Selector.sel_windowDidBecomeKey_.value;
public static final long sel_windowDidDeminiaturize_ = Selector.sel_windowDidDeminiaturize_.value;
public static final long sel_windowDidMiniaturize_ = Selector.sel_windowDidMiniaturize_.value;
public static final long sel_windowDidMove_ = Selector.sel_windowDidMove_.value;
public static final long sel_windowDidResignKey_ = Selector.sel_windowDidResignKey_.value;
public static final long sel_windowDidResize_ = Selector.sel_windowDidResize_.value;
public static final long sel_windowFrameTextColor = Selector.sel_windowFrameTextColor.value;
public static final long sel_windowNumber = Selector.sel_windowNumber.value;
public static final long sel_windowNumberAtPoint_belowWindowWithWindowNumber_ = Selector.sel_windowNumberAtPoint_belowWindowWithWindowNumber_.value;
public static final long sel_windowShouldClose_ = Selector.sel_windowShouldClose_.value;
public static final long sel_windowWillClose_ = Selector.sel_windowWillClose_.value;
public static final long sel_windowWithWindowNumber_ = Selector.sel_windowWithWindowNumber_.value;
public static final long sel_windows = Selector.sel_windows.value;
public static final long sel_writeObjects_ = Selector.sel_writeObjects_.value;
public static final long sel_writeSelectionToPasteboard_types_ = Selector.sel_writeSelectionToPasteboard_types_.value;
public static final long sel_yearOfCommonEra = Selector.sel_yearOfCommonEra.value;
public static final long sel_zoom_ = Selector.sel_zoom_.value;

/** Constants */
public static final int NSAlertFirstButtonReturn = 1000;
public static final int NSAlertSecondButtonReturn = 1001;
public static final int NSAlertThirdButtonReturn = 1002;
public static final int NSAlphaFirstBitmapFormat = 1;
public static final int NSAlphaNonpremultipliedBitmapFormat = 2;
public static final int NSAlternateKeyMask = 524288;
public static final long NSAnyEventMask = -1L;
public static final int NSAppKitDefined = 13;
public static final int NSApplicationActivateIgnoringOtherApps = 2;
public static final int NSApplicationActivationPolicyRegular = 0;
public static final int NSApplicationDefined = 15;
public static final int NSApplicationDelegateReplySuccess = 0;
public static final int NSAtTop = 2;
public static final int NSBackgroundStyleRaised = 2;
public static final int NSBackingStoreBuffered = 2;
public static final int NSBackspaceCharacter = 8;
public static final int NSBevelLineJoinStyle = 2;
public static final int NSBezelBorder = 2;
public static final int NSBoldFontMask = 2;
public static final int NSBorderlessWindowMask = 0;
public static final int NSBottomTabsBezelBorder = 2;
public static final int NSBoxCustom = 4;
public static final int NSBoxSeparator = 2;
public static final int NSButtLineCapStyle = 0;
public static final int NSCancelButton = 0;
public static final int NSCarriageReturnCharacter = 13;
public static final int NSClockAndCalendarDatePickerStyle = 1;
public static final int NSClosableWindowMask = 2;
public static final int NSClosePathBezierPathElement = 3;
public static final int NSCommandKeyMask = 1048576;
public static final int NSCompositeClear = 0;
public static final int NSCompositeCopy = 1;
public static final int NSCompositeSourceAtop = 5;
public static final int NSCompositeSourceOver = 2;
public static final int NSContentsCellMask = 1;
public static final int NSControlKeyMask = 262144;
public static final int NSCriticalAlertStyle = 2;
public static final int NSCurveToBezierPathElement = 2;
public static final int NSDeleteCharacter = 127;
public static final long NSDeviceIndependentModifierFlagsMask = 4294901760L;
public static final int NSDocModalWindowMask = 64;
public static final int NSDragOperationCopy = 1;
public static final int NSDragOperationDelete = 32;
public static final long NSDragOperationEvery = -1L;
public static final int NSDragOperationLink = 2;
public static final int NSDragOperationMove = 16;
public static final int NSDragOperationNone = 0;
public static final int NSEnterCharacter = 3;
public static final int NSEvenOddWindingRule = 1;
public static final int NSEventPhaseBegan = 1;
public static final int NSEventPhaseCancelled = 16;
public static final int NSEventPhaseEnded = 8;
public static final int NSEventTypeBeginGesture = 19;
public static final int NSEventTypeEndGesture = 20;
public static final int NSEventTypeGesture = 29;
public static final int NSEventTypeMagnify = 30;
public static final int NSEventTypeRotate = 18;
public static final int NSEventTypeSwipe = 31;
public static final int NSFileHandlingPanelOKButton = 1;
public static final int NSFlagsChanged = 12;
public static final int NSFocusRingTypeNone = 1;
public static final int NSFontPanelAllEffectsModeMask = 1048320;
public static final int NSFontPanelAllModesMask = -1;
public static final int NSFullScreenWindowMask = 16384;
public static final int NSHelpFunctionKey = 63302;
public static final int NSHelpKeyMask = 4194304;
public static final int NSHourMinuteDatePickerElementFlag = 12;
public static final int NSHourMinuteSecondDatePickerElementFlag = 14;
public static final int NSImageAbove = 5;
public static final int NSImageAlignCenter = 0;
public static final int NSImageAlignLeft = 4;
public static final int NSImageAlignRight = 8;
public static final int NSImageCacheNever = 3;
public static final int NSImageInterpolationDefault = 0;
public static final int NSImageInterpolationHigh = 3;
public static final int NSImageInterpolationLow = 2;
public static final int NSImageInterpolationNone = 1;
public static final int NSImageLeft = 2;
public static final int NSImageOnly = 1;
public static final int NSImageOverlaps = 6;
public static final int NSInformationalAlertStyle = 1;
public static final int NSItalicFontMask = 1;
public static final int NSKeyDown = 10;
public static final int NSKeyUp = 11;
public static final int NSLandscapeOrientation = 1;
public static final int NSLeftMouseDown = 1;
public static final int NSLeftMouseDragged = 6;
public static final int NSLeftMouseDraggedMask = 64;
public static final int NSLeftMouseUp = 2;
public static final int NSLeftMouseUpMask = 4;
public static final int NSLeftTabStopType = 0;
public static final int NSLineBreakByClipping = 2;
public static final int NSLineBreakByTruncatingMiddle = 5;
public static final int NSLineBreakByTruncatingTail = 4;
public static final int NSLineBreakByWordWrapping = 0;
public static final int NSLineToBezierPathElement = 1;
public static final int NSMiniControlSize = 2;
public static final int NSMiniaturizableWindowMask = 4;
public static final int NSMiterLineJoinStyle = 0;
public static final int NSMixedState = -1;
public static final int NSMomentaryLightButton = 0;
public static final int NSMouseEntered = 8;
public static final int NSMouseExited = 9;
public static final int NSMouseMoved = 5;
public static final int NSMoveToBezierPathElement = 0;
public static final int NSNewlineCharacter = 10;
public static final int NSNoBorder = 0;
public static final int NSNoImage = 0;
public static final int NSNoTitle = 0;
public static final int NSNonZeroWindingRule = 0;
public static final int NSNonactivatingPanelMask = 128;
public static final int NSOffState = 0;
public static final int NSOnState = 1;
public static final int NSOpenGLCPSurfaceOrder = 235;
public static final int NSOpenGLPFAAccumSize = 14;
public static final int NSOpenGLPFAAlphaSize = 11;
public static final int NSOpenGLPFAColorSize = 8;
public static final int NSOpenGLPFADepthSize = 12;
public static final int NSOpenGLPFADoubleBuffer = 5;
public static final int NSOpenGLPFASampleBuffers = 55;
public static final int NSOpenGLPFASamples = 56;
public static final int NSOpenGLPFAStencilSize = 13;
public static final int NSOpenGLPFAStereo = 6;
public static final int NSOtherMouseDown = 25;
public static final int NSOtherMouseDragged = 27;
public static final int NSOtherMouseUp = 26;
public static final int NSPageDownFunctionKey = 63277;
public static final int NSPageUpFunctionKey = 63276;
public static final int NSPortraitOrientation = 0;
public static final int NSPrintPanelShowsPageSetupAccessory = 256;
public static final int NSPrintPanelShowsPrintSelection = 32;
public static final int NSProgressIndicatorPreferredThickness = 14;
public static final int NSPushOnPushOffButton = 1;
public static final int NSRGBColorSpaceModel = 1;
public static final int NSRadioButton = 4;
public static final int NSRegularControlSize = 0;
public static final int NSRegularSquareBezelStyle = 2;
public static final int NSResizableWindowMask = 8;
public static final int NSRightMouseDown = 3;
public static final int NSRightMouseDragged = 7;
public static final int NSRightMouseUp = 4;
public static final int NSRoundLineCapStyle = 1;
public static final int NSRoundLineJoinStyle = 1;
public static final int NSRoundedBezelStyle = 1;
public static final int NSRoundedDisclosureBezelStyle = 14;
public static final int NSScaleNone = 2;
public static final int NSScrollElasticityNone = 1;
public static final int NSScrollWheel = 22;
public static final int NSScrollerDecrementLine = 4;
public static final int NSScrollerDecrementPage = 1;
public static final int NSScrollerIncrementLine = 5;
public static final int NSScrollerIncrementPage = 3;
public static final int NSScrollerKnob = 2;
public static final int NSScrollerKnobSlot = 6;
public static final int NSScrollerStyleLegacy = 0;
public static final int NSScrollerStyleOverlay = 1;
public static final int NSShadowlessSquareBezelStyle = 6;
public static final int NSShiftKeyMask = 131072;
public static final int NSSmallControlSize = 1;
public static final int NSSquareLineCapStyle = 2;
public static final int NSStatusWindowLevel = 25;
public static final int NSStringDrawingUsesLineFragmentOrigin = 1;
public static final int NSSubmenuWindowLevel = 3;
public static final int NSSwitchButton = 3;
public static final int NSSystemDefined = 14;
public static final int NSTabCharacter = 9;
public static final int NSTableColumnNoResizing = 0;
public static final int NSTableColumnUserResizingMask = 2;
public static final int NSTableViewDropAbove = 1;
public static final int NSTableViewDropOn = 0;
public static final int NSTableViewGridNone = 0;
public static final int NSTableViewNoColumnAutoresizing = 0;
public static final int NSTableViewSolidVerticalGridLineMask = 1;
public static final int NSTerminateCancel = 0;
public static final int NSTerminateNow = 1;
public static final int NSTextAlignmentCenter = 2;
public static final int NSTextAlignmentJustified = 3;
public static final int NSTextAlignmentLeft = 0;
public static final int NSTextAlignmentRight = 1;
public static final int NSTextFieldAndStepperDatePickerStyle = 0;
public static final int NSTextFieldDatePickerStyle = 2;
public static final int NSTitledWindowMask = 1;
public static final int NSToolbarDisplayModeIconOnly = 2;
public static final long NSTouchPhaseAny = -1L;
public static final int NSTouchPhaseBegan = 1;
public static final int NSTouchPhaseCancelled = 16;
public static final int NSTouchPhaseEnded = 8;
public static final int NSTouchPhaseMoved = 2;
public static final int NSTouchPhaseStationary = 4;
public static final int NSUnderlineStyleDouble = 9;
public static final int NSUnderlineStyleNone = 0;
public static final int NSUnderlineStyleSingle = 1;
public static final int NSUnderlineStyleThick = 2;
public static final int NSUtilityWindowMask = 16;
public static final int NSViewHeightSizable = 16;
public static final int NSViewMaxXMargin = 4;
public static final int NSViewMaxYMargin = 32;
public static final int NSViewMinXMargin = 1;
public static final int NSViewMinYMargin = 8;
public static final int NSViewWidthSizable = 2;
public static final int NSWarningAlertStyle = 0;
public static final int NSWindowAbove = 1;
public static final int NSWindowBelow = -1;
public static final int NSWindowCollectionBehaviorFullScreenAuxiliary = 256;
public static final int NSWindowCollectionBehaviorFullScreenPrimary = 128;
public static final int NSWindowCollectionBehaviorMoveToActiveSpace = 2;
public static final int NSWritingDirectionLeftToRight = 0;
public static final int NSWritingDirectionRightToLeft = 1;
public static final int NSYearMonthDatePickerElementFlag = 192;
public static final int NSYearMonthDayDatePickerElementFlag = 224;
public static final int kCFRunLoopBeforeWaiting = 32;
public static final int kCFStringEncodingUTF8 = 134217984;
public static final int kCGBlendModeDifference = 10;
public static final int kCGBlendModeNormal = 0;
public static final int kCGEventLeftMouseDown = 1;
public static final int kCGEventLeftMouseUp = 2;
public static final int kCGEventMouseMoved = 5;
public static final int kCGEventOtherMouseDown = 25;
public static final int kCGEventOtherMouseUp = 26;
public static final int kCGEventRightMouseDown = 3;
public static final int kCGEventRightMouseUp = 4;
public static final int kCGEventSourceStateHIDSystemState = 1;
public static final int kCGHIDEventTap = 0;
public static final int kCGImageAlphaFirst = 4;
public static final int kCGImageAlphaLast = 3;
public static final int kCGImageAlphaNoneSkipFirst = 6;
public static final int kCGImageAlphaNoneSkipLast = 5;
public static final int kCGImageAlphaOnly = 7;
public static final int kCGKeyboardEventKeyboardType = 10;
public static final int kCGLineCapButt = 0;
public static final int kCGLineCapRound = 1;
public static final int kCGLineCapSquare = 2;
public static final int kCGLineJoinBevel = 2;
public static final int kCGLineJoinMiter = 0;
public static final int kCGLineJoinRound = 1;
public static final int kCGPathElementAddCurveToPoint = 3;
public static final int kCGPathElementAddLineToPoint = 1;
public static final int kCGPathElementAddQuadCurveToPoint = 2;
public static final int kCGPathElementCloseSubpath = 4;
public static final int kCGPathElementMoveToPoint = 0;
public static final int kCGScrollEventUnitLine = 1;
public static final int kCTFontManagerScopeProcess = 1;
public static final int NSAllApplicationsDirectory = 100;
public static final int NSAllDomainsMask = 65535;
public static final int NSOrderedSame = 0;
public static final int NSURLCredentialPersistenceForSession = 1;
public static final int NSURLErrorBadURL = -1000;
public static final int NSURLErrorSecureConnectionFailed = -1200;
public static final int NSURLErrorServerCertificateNotYetValid = -1204;
public static final int NSURLRequestReloadIgnoringLocalCacheData = 1;
public static final int NSUTF8StringEncoding = 4;

/** Globals */
/** @method flags=const */
public static final native long NSAccessibilityAttributedStringForRangeParameterizedAttribute();
public static final NSString NSAccessibilityAttributedStringForRangeParameterizedAttribute = new NSString(NSAccessibilityAttributedStringForRangeParameterizedAttribute());
/** @method flags=const */
public static final native long NSAccessibilityBackgroundColorTextAttribute();
public static final NSString NSAccessibilityBackgroundColorTextAttribute = new NSString(NSAccessibilityBackgroundColorTextAttribute());
/** @method flags=const */
public static final native long NSAccessibilityBoundsForRangeParameterizedAttribute();
public static final NSString NSAccessibilityBoundsForRangeParameterizedAttribute = new NSString(NSAccessibilityBoundsForRangeParameterizedAttribute());
/** @method flags=const */
public static final native long NSAccessibilityButtonRole();
public static final NSString NSAccessibilityButtonRole = new NSString(NSAccessibilityButtonRole());
/** @method flags=const */
public static final native long NSAccessibilityCellForColumnAndRowParameterizedAttribute();
public static final NSString NSAccessibilityCellForColumnAndRowParameterizedAttribute = new NSString(NSAccessibilityCellForColumnAndRowParameterizedAttribute());
/** @method flags=const */
public static final native long NSAccessibilityCheckBoxRole();
public static final NSString NSAccessibilityCheckBoxRole = new NSString(NSAccessibilityCheckBoxRole());
/** @method flags=const */
public static final native long NSAccessibilityChildrenAttribute();
public static final NSString NSAccessibilityChildrenAttribute = new NSString(NSAccessibilityChildrenAttribute());
/** @method flags=const */
public static final native long NSAccessibilityColumnIndexRangeAttribute();
public static final NSString NSAccessibilityColumnIndexRangeAttribute = new NSString(NSAccessibilityColumnIndexRangeAttribute());
/** @method flags=const */
public static final native long NSAccessibilityColumnRole();
public static final NSString NSAccessibilityColumnRole = new NSString(NSAccessibilityColumnRole());
/** @method flags=const */
public static final native long NSAccessibilityColumnsAttribute();
public static final NSString NSAccessibilityColumnsAttribute = new NSString(NSAccessibilityColumnsAttribute());
/** @method flags=const */
public static final native long NSAccessibilityComboBoxRole();
public static final NSString NSAccessibilityComboBoxRole = new NSString(NSAccessibilityComboBoxRole());
/** @method flags=const */
public static final native long NSAccessibilityConfirmAction();
public static final NSString NSAccessibilityConfirmAction = new NSString(NSAccessibilityConfirmAction());
/** @method flags=const */
public static final native long NSAccessibilityContentsAttribute();
public static final NSString NSAccessibilityContentsAttribute = new NSString(NSAccessibilityContentsAttribute());
/** @method flags=const */
public static final native long NSAccessibilityDescriptionAttribute();
public static final NSString NSAccessibilityDescriptionAttribute = new NSString(NSAccessibilityDescriptionAttribute());
/** @method flags=const */
public static final native long NSAccessibilityDialogSubrole();
public static final NSString NSAccessibilityDialogSubrole = new NSString(NSAccessibilityDialogSubrole());
/** @method flags=const */
public static final native long NSAccessibilityEnabledAttribute();
public static final NSString NSAccessibilityEnabledAttribute = new NSString(NSAccessibilityEnabledAttribute());
/** @method flags=const */
public static final native long NSAccessibilityExpandedAttribute();
public static final NSString NSAccessibilityExpandedAttribute = new NSString(NSAccessibilityExpandedAttribute());
/** @method flags=const */
public static final native long NSAccessibilityFocusedAttribute();
public static final NSString NSAccessibilityFocusedAttribute = new NSString(NSAccessibilityFocusedAttribute());
/** @method flags=const */
public static final native long NSAccessibilityFocusedUIElementChangedNotification();
public static final NSString NSAccessibilityFocusedUIElementChangedNotification = new NSString(NSAccessibilityFocusedUIElementChangedNotification());
/** @method flags=const */
public static final native long NSAccessibilityFontFamilyKey();
public static final NSString NSAccessibilityFontFamilyKey = new NSString(NSAccessibilityFontFamilyKey());
/** @method flags=const */
public static final native long NSAccessibilityFontNameKey();
public static final NSString NSAccessibilityFontNameKey = new NSString(NSAccessibilityFontNameKey());
/** @method flags=const */
public static final native long NSAccessibilityFontSizeKey();
public static final NSString NSAccessibilityFontSizeKey = new NSString(NSAccessibilityFontSizeKey());
/** @method flags=const */
public static final native long NSAccessibilityFontTextAttribute();
public static final NSString NSAccessibilityFontTextAttribute = new NSString(NSAccessibilityFontTextAttribute());
/** @method flags=const */
public static final native long NSAccessibilityForegroundColorTextAttribute();
public static final NSString NSAccessibilityForegroundColorTextAttribute = new NSString(NSAccessibilityForegroundColorTextAttribute());
/** @method flags=const */
public static final native long NSAccessibilityGroupRole();
public static final NSString NSAccessibilityGroupRole = new NSString(NSAccessibilityGroupRole());
/** @method flags=const */
public static final native long NSAccessibilityHeaderAttribute();
public static final NSString NSAccessibilityHeaderAttribute = new NSString(NSAccessibilityHeaderAttribute());
/** @method flags=const */
public static final native long NSAccessibilityHelpAttribute();
public static final NSString NSAccessibilityHelpAttribute = new NSString(NSAccessibilityHelpAttribute());
/** @method flags=const */
public static final native long NSAccessibilityHelpTagRole();
public static final NSString NSAccessibilityHelpTagRole = new NSString(NSAccessibilityHelpTagRole());
/** @method flags=const */
public static final native long NSAccessibilityHorizontalOrientationValue();
public static final NSString NSAccessibilityHorizontalOrientationValue = new NSString(NSAccessibilityHorizontalOrientationValue());
/** @method flags=const */
public static final native long NSAccessibilityImageRole();
public static final NSString NSAccessibilityImageRole = new NSString(NSAccessibilityImageRole());
/** @method flags=const */
public static final native long NSAccessibilityIndexAttribute();
public static final NSString NSAccessibilityIndexAttribute = new NSString(NSAccessibilityIndexAttribute());
/** @method flags=const */
public static final native long NSAccessibilityInsertionPointLineNumberAttribute();
public static final NSString NSAccessibilityInsertionPointLineNumberAttribute = new NSString(NSAccessibilityInsertionPointLineNumberAttribute());
/** @method flags=const */
public static final native long NSAccessibilityLineForIndexParameterizedAttribute();
public static final NSString NSAccessibilityLineForIndexParameterizedAttribute = new NSString(NSAccessibilityLineForIndexParameterizedAttribute());
/** @method flags=const */
public static final native long NSAccessibilityLinkRole();
public static final NSString NSAccessibilityLinkRole = new NSString(NSAccessibilityLinkRole());
/** @method flags=const */
public static final native long NSAccessibilityLinkTextAttribute();
public static final NSString NSAccessibilityLinkTextAttribute = new NSString(NSAccessibilityLinkTextAttribute());
/** @method flags=const */
public static final native long NSAccessibilityMaxValueAttribute();
public static final NSString NSAccessibilityMaxValueAttribute = new NSString(NSAccessibilityMaxValueAttribute());
/** @method flags=const */
public static final native long NSAccessibilityMenuBarRole();
public static final NSString NSAccessibilityMenuBarRole = new NSString(NSAccessibilityMenuBarRole());
/** @method flags=const */
public static final native long NSAccessibilityMenuButtonRole();
public static final NSString NSAccessibilityMenuButtonRole = new NSString(NSAccessibilityMenuButtonRole());
/** @method flags=const */
public static final native long NSAccessibilityMenuItemRole();
public static final NSString NSAccessibilityMenuItemRole = new NSString(NSAccessibilityMenuItemRole());
/** @method flags=const */
public static final native long NSAccessibilityMenuRole();
public static final NSString NSAccessibilityMenuRole = new NSString(NSAccessibilityMenuRole());
/** @method flags=const */
public static final native long NSAccessibilityMinValueAttribute();
public static final NSString NSAccessibilityMinValueAttribute = new NSString(NSAccessibilityMinValueAttribute());
/** @method flags=const */
public static final native long NSAccessibilityMisspelledTextAttribute();
public static final NSString NSAccessibilityMisspelledTextAttribute = new NSString(NSAccessibilityMisspelledTextAttribute());
/** @method flags=const */
public static final native long NSAccessibilityMovedNotification();
public static final NSString NSAccessibilityMovedNotification = new NSString(NSAccessibilityMovedNotification());
/** @method flags=const */
public static final native long NSAccessibilityNextContentsAttribute();
public static final NSString NSAccessibilityNextContentsAttribute = new NSString(NSAccessibilityNextContentsAttribute());
/** @method flags=const */
public static final native long NSAccessibilityNumberOfCharactersAttribute();
public static final NSString NSAccessibilityNumberOfCharactersAttribute = new NSString(NSAccessibilityNumberOfCharactersAttribute());
/** @method flags=const */
public static final native long NSAccessibilityOrientationAttribute();
public static final NSString NSAccessibilityOrientationAttribute = new NSString(NSAccessibilityOrientationAttribute());
/** @method flags=const */
public static final native long NSAccessibilityOutlineRole();
public static final NSString NSAccessibilityOutlineRole = new NSString(NSAccessibilityOutlineRole());
/** @method flags=const */
public static final native long NSAccessibilityOutlineRowSubrole();
public static final NSString NSAccessibilityOutlineRowSubrole = new NSString(NSAccessibilityOutlineRowSubrole());
/** @method flags=const */
public static final native long NSAccessibilityParentAttribute();
public static final NSString NSAccessibilityParentAttribute = new NSString(NSAccessibilityParentAttribute());
/** @method flags=const */
public static final native long NSAccessibilityPositionAttribute();
public static final NSString NSAccessibilityPositionAttribute = new NSString(NSAccessibilityPositionAttribute());
/** @method flags=const */
public static final native long NSAccessibilityPressAction();
public static final NSString NSAccessibilityPressAction = new NSString(NSAccessibilityPressAction());
/** @method flags=const */
public static final native long NSAccessibilityPreviousContentsAttribute();
public static final NSString NSAccessibilityPreviousContentsAttribute = new NSString(NSAccessibilityPreviousContentsAttribute());
/** @method flags=const */
public static final native long NSAccessibilityProgressIndicatorRole();
public static final NSString NSAccessibilityProgressIndicatorRole = new NSString(NSAccessibilityProgressIndicatorRole());
/** @method flags=const */
public static final native long NSAccessibilityRadioButtonRole();
public static final NSString NSAccessibilityRadioButtonRole = new NSString(NSAccessibilityRadioButtonRole());
/** @method flags=const */
public static final native long NSAccessibilityRangeForIndexParameterizedAttribute();
public static final NSString NSAccessibilityRangeForIndexParameterizedAttribute = new NSString(NSAccessibilityRangeForIndexParameterizedAttribute());
/** @method flags=const */
public static final native long NSAccessibilityRangeForLineParameterizedAttribute();
public static final NSString NSAccessibilityRangeForLineParameterizedAttribute = new NSString(NSAccessibilityRangeForLineParameterizedAttribute());
/** @method flags=const */
public static final native long NSAccessibilityRangeForPositionParameterizedAttribute();
public static final NSString NSAccessibilityRangeForPositionParameterizedAttribute = new NSString(NSAccessibilityRangeForPositionParameterizedAttribute());
/** @method flags=const */
public static final native long NSAccessibilityRoleAttribute();
public static final NSString NSAccessibilityRoleAttribute = new NSString(NSAccessibilityRoleAttribute());
/** @method flags=const */
public static final native long NSAccessibilityRoleDescriptionAttribute();
public static final NSString NSAccessibilityRoleDescriptionAttribute = new NSString(NSAccessibilityRoleDescriptionAttribute());
/** @method flags=const */
public static final native long NSAccessibilityRowCountChangedNotification();
public static final NSString NSAccessibilityRowCountChangedNotification = new NSString(NSAccessibilityRowCountChangedNotification());
/** @method flags=const */
public static final native long NSAccessibilityRowIndexRangeAttribute();
public static final NSString NSAccessibilityRowIndexRangeAttribute = new NSString(NSAccessibilityRowIndexRangeAttribute());
/** @method flags=const */
public static final native long NSAccessibilityRowRole();
public static final NSString NSAccessibilityRowRole = new NSString(NSAccessibilityRowRole());
/** @method flags=const */
public static final native long NSAccessibilityRowsAttribute();
public static final NSString NSAccessibilityRowsAttribute = new NSString(NSAccessibilityRowsAttribute());
/** @method flags=const */
public static final native long NSAccessibilityScrollBarRole();
public static final NSString NSAccessibilityScrollBarRole = new NSString(NSAccessibilityScrollBarRole());
/** @method flags=const */
public static final native long NSAccessibilitySelectedAttribute();
public static final NSString NSAccessibilitySelectedAttribute = new NSString(NSAccessibilitySelectedAttribute());
/** @method flags=const */
public static final native long NSAccessibilitySelectedChildrenAttribute();
public static final NSString NSAccessibilitySelectedChildrenAttribute = new NSString(NSAccessibilitySelectedChildrenAttribute());
/** @method flags=const */
public static final native long NSAccessibilitySelectedChildrenChangedNotification();
public static final NSString NSAccessibilitySelectedChildrenChangedNotification = new NSString(NSAccessibilitySelectedChildrenChangedNotification());
/** @method flags=const */
public static final native long NSAccessibilitySelectedColumnsAttribute();
public static final NSString NSAccessibilitySelectedColumnsAttribute = new NSString(NSAccessibilitySelectedColumnsAttribute());
/** @method flags=const */
public static final native long NSAccessibilitySelectedRowsAttribute();
public static final NSString NSAccessibilitySelectedRowsAttribute = new NSString(NSAccessibilitySelectedRowsAttribute());
/** @method flags=const */
public static final native long NSAccessibilitySelectedRowsChangedNotification();
public static final NSString NSAccessibilitySelectedRowsChangedNotification = new NSString(NSAccessibilitySelectedRowsChangedNotification());
/** @method flags=const */
public static final native long NSAccessibilitySelectedTextAttribute();
public static final NSString NSAccessibilitySelectedTextAttribute = new NSString(NSAccessibilitySelectedTextAttribute());
/** @method flags=const */
public static final native long NSAccessibilitySelectedTextChangedNotification();
public static final NSString NSAccessibilitySelectedTextChangedNotification = new NSString(NSAccessibilitySelectedTextChangedNotification());
/** @method flags=const */
public static final native long NSAccessibilitySelectedTextRangeAttribute();
public static final NSString NSAccessibilitySelectedTextRangeAttribute = new NSString(NSAccessibilitySelectedTextRangeAttribute());
/** @method flags=const */
public static final native long NSAccessibilitySelectedTextRangesAttribute();
public static final NSString NSAccessibilitySelectedTextRangesAttribute = new NSString(NSAccessibilitySelectedTextRangesAttribute());
/** @method flags=const */
public static final native long NSAccessibilityServesAsTitleForUIElementsAttribute();
public static final NSString NSAccessibilityServesAsTitleForUIElementsAttribute = new NSString(NSAccessibilityServesAsTitleForUIElementsAttribute());
/** @method flags=const */
public static final native long NSAccessibilityShowMenuAction();
public static final NSString NSAccessibilityShowMenuAction = new NSString(NSAccessibilityShowMenuAction());
/** @method flags=const */
public static final native long NSAccessibilitySizeAttribute();
public static final NSString NSAccessibilitySizeAttribute = new NSString(NSAccessibilitySizeAttribute());
/** @method flags=const */
public static final native long NSAccessibilitySliderRole();
public static final NSString NSAccessibilitySliderRole = new NSString(NSAccessibilitySliderRole());
/** @method flags=const */
public static final native long NSAccessibilitySplitterRole();
public static final NSString NSAccessibilitySplitterRole = new NSString(NSAccessibilitySplitterRole());
/** @method flags=const */
public static final native long NSAccessibilityStaticTextRole();
public static final NSString NSAccessibilityStaticTextRole = new NSString(NSAccessibilityStaticTextRole());
/** @method flags=const */
public static final native long NSAccessibilityStrikethroughColorTextAttribute();
public static final NSString NSAccessibilityStrikethroughColorTextAttribute = new NSString(NSAccessibilityStrikethroughColorTextAttribute());
/** @method flags=const */
public static final native long NSAccessibilityStrikethroughTextAttribute();
public static final NSString NSAccessibilityStrikethroughTextAttribute = new NSString(NSAccessibilityStrikethroughTextAttribute());
/** @method flags=const */
public static final native long NSAccessibilityStringForRangeParameterizedAttribute();
public static final NSString NSAccessibilityStringForRangeParameterizedAttribute = new NSString(NSAccessibilityStringForRangeParameterizedAttribute());
/** @method flags=const */
public static final native long NSAccessibilityStyleRangeForIndexParameterizedAttribute();
public static final NSString NSAccessibilityStyleRangeForIndexParameterizedAttribute = new NSString(NSAccessibilityStyleRangeForIndexParameterizedAttribute());
/** @method flags=const */
public static final native long NSAccessibilitySubroleAttribute();
public static final NSString NSAccessibilitySubroleAttribute = new NSString(NSAccessibilitySubroleAttribute());
/** @method flags=const */
public static final native long NSAccessibilityTabGroupRole();
public static final NSString NSAccessibilityTabGroupRole = new NSString(NSAccessibilityTabGroupRole());
/** @method flags=const */
public static final native long NSAccessibilityTableRole();
public static final NSString NSAccessibilityTableRole = new NSString(NSAccessibilityTableRole());
/** @method flags=const */
public static final native long NSAccessibilityTableRowSubrole();
public static final NSString NSAccessibilityTableRowSubrole = new NSString(NSAccessibilityTableRowSubrole());
/** @method flags=const */
public static final native long NSAccessibilityTabsAttribute();
public static final NSString NSAccessibilityTabsAttribute = new NSString(NSAccessibilityTabsAttribute());
/** @method flags=const */
public static final native long NSAccessibilityTextAreaRole();
public static final NSString NSAccessibilityTextAreaRole = new NSString(NSAccessibilityTextAreaRole());
/** @method flags=const */
public static final native long NSAccessibilityTextFieldRole();
public static final NSString NSAccessibilityTextFieldRole = new NSString(NSAccessibilityTextFieldRole());
/** @method flags=const */
public static final native long NSAccessibilityTitleAttribute();
public static final NSString NSAccessibilityTitleAttribute = new NSString(NSAccessibilityTitleAttribute());
/** @method flags=const */
public static final native long NSAccessibilityTitleChangedNotification();
public static final NSString NSAccessibilityTitleChangedNotification = new NSString(NSAccessibilityTitleChangedNotification());
/** @method flags=const */
public static final native long NSAccessibilityTitleUIElementAttribute();
public static final NSString NSAccessibilityTitleUIElementAttribute = new NSString(NSAccessibilityTitleUIElementAttribute());
/** @method flags=const */
public static final native long NSAccessibilityToolbarRole();
public static final NSString NSAccessibilityToolbarRole = new NSString(NSAccessibilityToolbarRole());
/** @method flags=const */
public static final native long NSAccessibilityTopLevelUIElementAttribute();
public static final NSString NSAccessibilityTopLevelUIElementAttribute = new NSString(NSAccessibilityTopLevelUIElementAttribute());
/** @method flags=const */
public static final native long NSAccessibilityUnderlineColorTextAttribute();
public static final NSString NSAccessibilityUnderlineColorTextAttribute = new NSString(NSAccessibilityUnderlineColorTextAttribute());
/** @method flags=const */
public static final native long NSAccessibilityUnderlineTextAttribute();
public static final NSString NSAccessibilityUnderlineTextAttribute = new NSString(NSAccessibilityUnderlineTextAttribute());
/** @method flags=const */
public static final native long NSAccessibilityUnknownRole();
public static final NSString NSAccessibilityUnknownRole = new NSString(NSAccessibilityUnknownRole());
/** @method flags=const */
public static final native long NSAccessibilityUnknownSubrole();
public static final NSString NSAccessibilityUnknownSubrole = new NSString(NSAccessibilityUnknownSubrole());
/** @method flags=const */
public static final native long NSAccessibilityValueAttribute();
public static final NSString NSAccessibilityValueAttribute = new NSString(NSAccessibilityValueAttribute());
/** @method flags=const */
public static final native long NSAccessibilityValueChangedNotification();
public static final NSString NSAccessibilityValueChangedNotification = new NSString(NSAccessibilityValueChangedNotification());
/** @method flags=const */
public static final native long NSAccessibilityVerticalOrientationValue();
public static final NSString NSAccessibilityVerticalOrientationValue = new NSString(NSAccessibilityVerticalOrientationValue());
/** @method flags=const */
public static final native long NSAccessibilityVisibleCharacterRangeAttribute();
public static final NSString NSAccessibilityVisibleCharacterRangeAttribute = new NSString(NSAccessibilityVisibleCharacterRangeAttribute());
/** @method flags=const */
public static final native long NSAccessibilityVisibleChildrenAttribute();
public static final NSString NSAccessibilityVisibleChildrenAttribute = new NSString(NSAccessibilityVisibleChildrenAttribute());
/** @method flags=const */
public static final native long NSAccessibilityVisibleColumnsAttribute();
public static final NSString NSAccessibilityVisibleColumnsAttribute = new NSString(NSAccessibilityVisibleColumnsAttribute());
/** @method flags=const */
public static final native long NSAccessibilityVisibleNameKey();
public static final NSString NSAccessibilityVisibleNameKey = new NSString(NSAccessibilityVisibleNameKey());
/** @method flags=const */
public static final native long NSAccessibilityVisibleRowsAttribute();
public static final NSString NSAccessibilityVisibleRowsAttribute = new NSString(NSAccessibilityVisibleRowsAttribute());
/** @method flags=const */
public static final native long NSAccessibilityWindowAttribute();
public static final NSString NSAccessibilityWindowAttribute = new NSString(NSAccessibilityWindowAttribute());
/** @method flags=const */
public static final native long NSAccessibilityWindowRole();
public static final NSString NSAccessibilityWindowRole = new NSString(NSAccessibilityWindowRole());
/** @method flags=const */
public static final native long NSApplicationDidChangeScreenParametersNotification();
public static final NSString NSApplicationDidChangeScreenParametersNotification = new NSString(NSApplicationDidChangeScreenParametersNotification());
/** @method flags=const */
public static final native long NSAttachmentAttributeName();
public static final NSString NSAttachmentAttributeName = new NSString(NSAttachmentAttributeName());
/** @method flags=const */
public static final native long NSBackgroundColorAttributeName();
public static final NSString NSBackgroundColorAttributeName = new NSString(NSBackgroundColorAttributeName());
/** @method flags=const */
public static final native long NSBaselineOffsetAttributeName();
public static final NSString NSBaselineOffsetAttributeName = new NSString(NSBaselineOffsetAttributeName());
/** @method flags=const */
public static final native long NSCursorAttributeName();
public static final NSString NSCursorAttributeName = new NSString(NSCursorAttributeName());
/** @method flags=const */
public static final native long NSDeviceRGBColorSpace();
public static final NSString NSDeviceRGBColorSpace = new NSString(NSDeviceRGBColorSpace());
/** @method flags=const */
public static final native long NSDeviceResolution();
public static final NSString NSDeviceResolution = new NSString(NSDeviceResolution());
/** @method flags=const */
public static final native long NSDragPboard();
public static final NSString NSDragPboard = new NSString(NSDragPboard());
/** @method flags=const */
public static final native long NSEventTrackingRunLoopMode();
public static final NSString NSEventTrackingRunLoopMode = new NSString(NSEventTrackingRunLoopMode());
/** @method flags=const */
public static final native long NSFilenamesPboardType();
public static final NSString NSFilenamesPboardType = new NSString(NSFilenamesPboardType());
/** @method flags=const */
public static final native long NSFontAttributeName();
public static final NSString NSFontAttributeName = new NSString(NSFontAttributeName());
/** @method flags=const */
public static final native long NSForegroundColorAttributeName();
public static final NSString NSForegroundColorAttributeName = new NSString(NSForegroundColorAttributeName());
/** @method flags=const */
public static final native long NSLigatureAttributeName();
public static final NSString NSLigatureAttributeName = new NSString(NSLigatureAttributeName());
/** @method flags=const */
public static final native long NSLinkAttributeName();
public static final NSString NSLinkAttributeName = new NSString(NSLinkAttributeName());
/** @method flags=const */
public static final native long NSModalPanelRunLoopMode();
public static final NSString NSModalPanelRunLoopMode = new NSString(NSModalPanelRunLoopMode());
/** @method flags=const */
public static final native long NSObliquenessAttributeName();
public static final NSString NSObliquenessAttributeName = new NSString(NSObliquenessAttributeName());
/** @method flags=const */
public static final native long NSParagraphStyleAttributeName();
public static final NSString NSParagraphStyleAttributeName = new NSString(NSParagraphStyleAttributeName());
/** @method flags=const */
public static final native long NSPasteboardTypeHTML();
public static final NSString NSPasteboardTypeHTML = new NSString(NSPasteboardTypeHTML());
/** @method flags=const */
public static final native long NSPasteboardTypeRTF();
public static final NSString NSPasteboardTypeRTF = new NSString(NSPasteboardTypeRTF());
/** @method flags=const */
public static final native long NSPasteboardTypeString();
public static final NSString NSPasteboardTypeString = new NSString(NSPasteboardTypeString());
/** @method flags=const */
public static final native long NSPrintAllPages();
public static final NSString NSPrintAllPages = new NSString(NSPrintAllPages());
/** @method flags=const */
public static final native long NSPrintCopies();
public static final NSString NSPrintCopies = new NSString(NSPrintCopies());
/** @method flags=const */
public static final native long NSPrintFirstPage();
public static final NSString NSPrintFirstPage = new NSString(NSPrintFirstPage());
/** @method flags=const */
public static final native long NSPrintJobDisposition();
public static final NSString NSPrintJobDisposition = new NSString(NSPrintJobDisposition());
/** @method flags=const */
public static final native long NSPrintLastPage();
public static final NSString NSPrintLastPage = new NSString(NSPrintLastPage());
/** @method flags=const */
public static final native long NSPrintMustCollate();
public static final NSString NSPrintMustCollate = new NSString(NSPrintMustCollate());
/** @method flags=const */
public static final native long NSPrintOrientation();
public static final NSString NSPrintOrientation = new NSString(NSPrintOrientation());
/** @method flags=const */
public static final native long NSPrintPreviewJob();
public static final NSString NSPrintPreviewJob = new NSString(NSPrintPreviewJob());
/** @method flags=const */
public static final native long NSPrintSaveJob();
public static final NSString NSPrintSaveJob = new NSString(NSPrintSaveJob());
/** @method flags=const */
public static final native long NSPrintSavePath();
public static final NSString NSPrintSavePath = new NSString(NSPrintSavePath());
/** @method flags=const */
public static final native long NSPrintScalingFactor();
public static final NSString NSPrintScalingFactor = new NSString(NSPrintScalingFactor());
/** @method flags=const */
public static final native double NSSquareStatusItemLength();
/** @method flags=const */
public static final native long NSStrikethroughColorAttributeName();
public static final NSString NSStrikethroughColorAttributeName = new NSString(NSStrikethroughColorAttributeName());
/** @method flags=const */
public static final native long NSStrikethroughStyleAttributeName();
public static final NSString NSStrikethroughStyleAttributeName = new NSString(NSStrikethroughStyleAttributeName());
/** @method flags=const */
public static final native long NSStrokeWidthAttributeName();
public static final NSString NSStrokeWidthAttributeName = new NSString(NSStrokeWidthAttributeName());
/** @method flags=const */
public static final native long NSSystemColorsDidChangeNotification();
public static final NSString NSSystemColorsDidChangeNotification = new NSString(NSSystemColorsDidChangeNotification());
/** @method flags=const */
public static final native long NSTIFFPboardType();
public static final NSString NSTIFFPboardType = new NSString(NSTIFFPboardType());
/** @method flags=const */
public static final native long NSToolbarFlexibleSpaceItemIdentifier();
public static final NSString NSToolbarFlexibleSpaceItemIdentifier = new NSString(NSToolbarFlexibleSpaceItemIdentifier());
/** @method flags=const */
public static final native long NSToolbarSpaceItemIdentifier();
public static final NSString NSToolbarSpaceItemIdentifier = new NSString(NSToolbarSpaceItemIdentifier());
/** @method flags=const */
public static final native long NSURLPboardType();
public static final NSString NSURLPboardType = new NSString(NSURLPboardType());
/** @method flags=const */
public static final native long NSUnderlineColorAttributeName();
public static final NSString NSUnderlineColorAttributeName = new NSString(NSUnderlineColorAttributeName());
/** @method flags=const */
public static final native long NSUnderlineStyleAttributeName();
public static final NSString NSUnderlineStyleAttributeName = new NSString(NSUnderlineStyleAttributeName());
/** @method flags=const */
public static final native long NSViewGlobalFrameDidChangeNotification();
public static final NSString NSViewGlobalFrameDidChangeNotification = new NSString(NSViewGlobalFrameDidChangeNotification());
/** @method flags=const */
public static final native long NSWindowDidBecomeKeyNotification();
public static final NSString NSWindowDidBecomeKeyNotification = new NSString(NSWindowDidBecomeKeyNotification());
/** @method flags=const */
public static final native long NSWindowDidDeminiaturizeNotification();
public static final NSString NSWindowDidDeminiaturizeNotification = new NSString(NSWindowDidDeminiaturizeNotification());
/** @method flags=const */
public static final native long NSWindowDidMiniaturizeNotification();
public static final NSString NSWindowDidMiniaturizeNotification = new NSString(NSWindowDidMiniaturizeNotification());
/** @method flags=const */
public static final native long NSWindowDidMoveNotification();
public static final NSString NSWindowDidMoveNotification = new NSString(NSWindowDidMoveNotification());
/** @method flags=const */
public static final native long NSWindowDidResignKeyNotification();
public static final NSString NSWindowDidResignKeyNotification = new NSString(NSWindowDidResignKeyNotification());
/** @method flags=const */
public static final native long NSWindowDidResizeNotification();
public static final NSString NSWindowDidResizeNotification = new NSString(NSWindowDidResizeNotification());
/** @method flags=const */
public static final native long NSWindowWillCloseNotification();
public static final NSString NSWindowWillCloseNotification = new NSString(NSWindowWillCloseNotification());
/** @method flags=const */
public static final native long kCFAllocatorDefault();
/** @method flags=const */
public static final native long kCFRunLoopCommonModes();
/** @method flags=const */
public static final native long NSDefaultRunLoopMode();
public static final NSString NSDefaultRunLoopMode = new NSString(NSDefaultRunLoopMode());
/** @method flags=const */
public static final native long NSLocaleLanguageCode();
public static final NSString NSLocaleLanguageCode = new NSString(NSLocaleLanguageCode());
/** @method flags=const */
public static final native long NSNotFound();

/** Functions */

/**
 * @param element cast=(id)
 * @param notification cast=(NSString*)
 */
public static final native void NSAccessibilityPostNotification(long element, long notification);
/**
 * @param role cast=(NSString*)
 * @param subrole cast=(NSString*)
 */
public static final native long NSAccessibilityRoleDescription(long role, long subrole);
/**
 * @param element cast=(id)
 */
public static final native long NSAccessibilityUnignoredAncestor(long element);
/**
 * @param originalChildren cast=(NSArray*)
 */
public static final native long NSAccessibilityUnignoredChildren(long originalChildren);
public static final native void NSBeep();
/**
 * @param depth cast=(NSWindowDepth)
 */
public static final native long NSBitsPerPixelFromDepth(int depth);
/**
 * @param aRect flags=struct
 * @param op cast=(NSCompositingOperation)
 */
public static final native void NSRectFillUsingOperation(NSRect aRect, long op);
/**
 * @param theData cast=(CFDataRef)
 */
public static final native long CFDataGetBytePtr(long theData);
/**
 * @param theData cast=(CFDataRef)
 */
public static final native long CFDataGetLength(long theData);
/**
 * @param cf cast=(CFTypeRef)
 */
public static final native void CFRelease(long cf);
/**
 * @param rl cast=(CFRunLoopRef)
 * @param observer cast=(CFRunLoopObserverRef)
 * @param mode cast=(CFStringRef)
 */
public static final native void CFRunLoopAddObserver(long rl, long observer, long mode);
public static final native long CFRunLoopGetCurrent();
/**
 * @param allocator cast=(CFAllocatorRef)
 * @param activities cast=(CFOptionFlags)
 * @param repeats cast=(Boolean)
 * @param order cast=(CFIndex)
 * @param callout cast=(CFRunLoopObserverCallBack)
 * @param context cast=(CFRunLoopObserverContext*)
 */
public static final native long CFRunLoopObserverCreate(long allocator, long activities, boolean repeats, long order, long callout, long context);
/**
 * @param observer cast=(CFRunLoopObserverRef)
 */
public static final native void CFRunLoopObserverInvalidate(long observer);
/**
 * @param allocator cast=(CFAllocatorRef)
 * @param fsRef cast=(FSRef*)
 */
public static final native long CFURLCreateFromFSRef(long allocator, byte[] fsRef);
/**
 * @param allocator cast=(CFAllocatorRef)
 * @param originalString cast=(CFStringRef)
 * @param charactersToLeaveUnescaped cast=(CFStringRef)
 * @param legalURLCharactersToBeEscaped cast=(CFStringRef)
 * @param encoding cast=(CFStringEncoding)
 */
public static final native long CFURLCreateStringByAddingPercentEscapes(long allocator, long originalString, long charactersToLeaveUnescaped, long legalURLCharactersToBeEscaped, int encoding);
/**
 * @param data cast=(void*)
 * @param width cast=(size_t)
 * @param height cast=(size_t)
 * @param bitsPerComponent cast=(size_t)
 * @param bytesPerRow cast=(size_t)
 * @param space cast=(CGColorSpaceRef)
 * @param bitmapInfo cast=(CGBitmapInfo)
 */
public static final native long CGBitmapContextCreate(long data, long width, long height, long bitsPerComponent, long bytesPerRow, long space, int bitmapInfo);
/**
 * @param space cast=(CGColorSpaceRef)
 * @param components cast=(CGFloat*)
 */
public static final native long CGColorCreate(long space, double[] components);
/**
 * @param color cast=(CGColorRef)
 */
public static final native void CGColorRelease(long color);
public static final native long CGColorSpaceCreateDeviceRGB();
/**
 * @param space cast=(CGColorSpaceRef)
 */
public static final native void CGColorSpaceRelease(long space);
/**
 * @param context cast=(CGContextRef)
 * @param path cast=(CGPathRef)
 */
public static final native void CGContextAddPath(long context, long path);
/**
 * @param context cast=(CGContextRef)
 * @param rect flags=struct
 * @param auxiliaryInfo cast=(CFDictionaryRef)
 */
public static final native void CGContextBeginTransparencyLayerWithRect(long context, CGRect rect, long auxiliaryInfo);
/**
 * @param context cast=(CGContextRef)
 */
public static final native long CGContextCopyPath(long context);
/**
 * @param c cast=(CGContextRef)
 * @param rect flags=struct
 * @param image cast=(CGImageRef)
 */
public static final native void CGContextDrawImage(long c, CGRect rect, long image);
/**
 * @param context cast=(CGContextRef)
 */
public static final native void CGContextEndTransparencyLayer(long context);
/**
 * @param c cast=(CGContextRef)
 * @param rect flags=struct
 */
public static final native void CGContextFillRect(long c, CGRect rect);
/**
 * @param c cast=(CGContextRef)
 */
public static final native void CGContextRelease(long c);
/**
 * @param c cast=(CGContextRef)
 */
public static final native void CGContextReplacePathWithStrokedPath(long c);
/**
 * @param c cast=(CGContextRef)
 */
public static final native void CGContextRestoreGState(long c);
/**
 * @param c cast=(CGContextRef)
 */
public static final native void CGContextSaveGState(long c);
/**
 * @param c cast=(CGContextRef)
 * @param sx cast=(CGFloat)
 * @param sy cast=(CGFloat)
 */
public static final native void CGContextScaleCTM(long c, double sx, double sy);
/**
 * @param context cast=(CGContextRef)
 * @param mode cast=(CGBlendMode)
 */
public static final native void CGContextSetBlendMode(long context, int mode);
/**
 * @param context cast=(CGContextRef)
 * @param components cast=(CGFloat*)
 */
public static final native void CGContextSetFillColor(long context, double[] components);
/**
 * @param context cast=(CGContextRef)
 * @param space cast=(CGColorSpaceRef)
 */
public static final native void CGContextSetFillColorSpace(long context, long space);
/**
 * @param c cast=(CGContextRef)
 * @param cap cast=(CGLineCap)
 */
public static final native void CGContextSetLineCap(long c, int cap);
/**
 * @param c cast=(CGContextRef)
 * @param phase cast=(CGFloat)
 * @param lengths cast=(CGFloat*)
 * @param count cast=(size_t)
 */
public static final native void CGContextSetLineDash(long c, double phase, double[] lengths, long count);
/**
 * @param c cast=(CGContextRef)
 * @param join cast=(CGLineJoin)
 */
public static final native void CGContextSetLineJoin(long c, int join);
/**
 * @param c cast=(CGContextRef)
 * @param width cast=(CGFloat)
 */
public static final native void CGContextSetLineWidth(long c, double width);
/**
 * @param c cast=(CGContextRef)
 * @param limit cast=(CGFloat)
 */
public static final native void CGContextSetMiterLimit(long c, double limit);
/**
 * @param c cast=(CGContextRef)
 */
public static final native void CGContextStrokePath(long c);
/**
 * @param c cast=(CGContextRef)
 * @param tx cast=(CGFloat)
 * @param ty cast=(CGFloat)
 */
public static final native void CGContextTranslateCTM(long c, double tx, double ty);
/**
 * @param info cast=(void*)
 * @param data cast=(void*)
 * @param size cast=(size_t)
 * @param releaseData cast=(CGDataProviderReleaseDataCallback)
 */
public static final native long CGDataProviderCreateWithData(long info, long data, long size, long releaseData);
/**
 * @param provider cast=(CGDataProviderRef)
 */
public static final native void CGDataProviderRelease(long provider);
/**
 * @param displayID cast=(CGDirectDisplayID)
 */
public static final native long CGDisplayCreateImage(int displayID);
/**
 * @param source cast=(CGEventSourceRef)
 * @param virtualKey cast=(CGKeyCode)
 * @param keyDown cast=(_Bool)
 */
public static final native long CGEventCreateKeyboardEvent(long source, short virtualKey, boolean keyDown);
/**
 * @param source cast=(CGEventSourceRef)
 * @param mouseType cast=(CGEventType)
 * @param mouseCursorPosition flags=struct
 * @param mouseButton cast=(CGMouseButton)
 */
public static final native long CGEventCreateMouseEvent(long source, int mouseType, CGPoint mouseCursorPosition, int mouseButton);
/**
 * @param source cast=(CGEventSourceRef)
 * @param units cast=(CGScrollEventUnit)
 * @param wheelCount cast=(uint32_t)
 * @param wheel1 cast=(int32_t)
 */
public static final native long CGEventCreateScrollWheelEvent(long source, int units, int wheelCount, int wheel1);
/**
 * @param event cast=(CGEventRef)
 * @param field cast=(CGEventField)
 */
public static final native long CGEventGetIntegerValueField(long event, int field);
/**
 * @param tap cast=(CGEventTapLocation)
 * @param event cast=(CGEventRef)
 */
public static final native void CGEventPost(int tap, long event);
/**
 * @param stateID cast=(CGEventSourceStateID)
 */
public static final native long CGEventSourceCreate(int stateID);
/**
 * @param rect flags=struct
 * @param maxDisplays cast=(uint32_t)
 * @param displays cast=(CGDirectDisplayID*)
 * @param matchingDisplayCount cast=(uint32_t*)
 */
public static final native int CGGetDisplaysWithRect(CGRect rect, int maxDisplays, long displays, long matchingDisplayCount);
/**
 * @param width cast=(size_t)
 * @param height cast=(size_t)
 * @param bitsPerComponent cast=(size_t)
 * @param bitsPerPixel cast=(size_t)
 * @param bytesPerRow cast=(size_t)
 * @param space cast=(CGColorSpaceRef)
 * @param bitmapInfo cast=(CGBitmapInfo)
 * @param provider cast=(CGDataProviderRef)
 * @param decode cast=(CGFloat*)
 * @param shouldInterpolate cast=(_Bool)
 * @param intent cast=(CGColorRenderingIntent)
 */
public static final native long CGImageCreate(long width, long height, long bitsPerComponent, long bitsPerPixel, long bytesPerRow, long space, int bitmapInfo, long provider, long decode, boolean shouldInterpolate, int intent);
/**
 * @param image cast=(CGImageRef)
 */
public static final native long CGImageGetHeight(long image);
/**
 * @param image cast=(CGImageRef)
 */
public static final native long CGImageGetWidth(long image);
/**
 * @param image cast=(CGImageRef)
 */
public static final native void CGImageRelease(long image);
/**
 * @param path cast=(CGMutablePathRef)
 * @param m cast=(CGAffineTransform*)
 * @param cp1x cast=(CGFloat)
 * @param cp1y cast=(CGFloat)
 * @param cp2x cast=(CGFloat)
 * @param cp2y cast=(CGFloat)
 * @param x cast=(CGFloat)
 * @param y cast=(CGFloat)
 */
public static final native void CGPathAddCurveToPoint(long path, long m, double cp1x, double cp1y, double cp2x, double cp2y, double x, double y);
/**
 * @param path cast=(CGMutablePathRef)
 * @param m cast=(CGAffineTransform*)
 * @param x cast=(CGFloat)
 * @param y cast=(CGFloat)
 */
public static final native void CGPathAddLineToPoint(long path, long m, double x, double y);
/**
 * @param path cast=(CGPathRef)
 * @param info cast=(void*)
 * @param function cast=(CGPathApplierFunction)
 */
public static final native void CGPathApply(long path, long info, long function);
/**
 * @param path cast=(CGMutablePathRef)
 */
public static final native void CGPathCloseSubpath(long path);
public static final native long CGPathCreateMutable();
/**
 * @param path cast=(CGMutablePathRef)
 * @param m cast=(CGAffineTransform*)
 * @param x cast=(CGFloat)
 * @param y cast=(CGFloat)
 */
public static final native void CGPathMoveToPoint(long path, long m, double x, double y);
/**
 * @param path cast=(CGPathRef)
 */
public static final native void CGPathRelease(long path);
/**
 * @param fontURL cast=(CFURLRef)
 * @param scope cast=(CTFontManagerScope)
 * @param error cast=(CFErrorRef*)
 */
public static final native boolean CTFontManagerRegisterFontsForURL(long fontURL, int scope, long error);
/**
 * @param aRect flags=struct
 * @param bRect flags=struct
 */
public static final native boolean NSEqualRects(NSRect aRect, NSRect bRect);
/**
 * @param aPoint flags=struct
 * @param aRect flags=struct
 */
public static final native boolean NSPointInRect(NSPoint aPoint, NSRect aRect);
/**
 * @param directory cast=(NSSearchPathDirectory)
 * @param domainMask cast=(NSSearchPathDomainMask)
 * @param expandTilde cast=(BOOL)
 */
public static final native long NSSearchPathForDirectoriesInDomains(long directory, long domainMask, boolean expandTilde);

/** Super Sends */

/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native boolean objc_msgSendSuper_bool(objc_super superId, long sel, NSRange arg0, long arg1);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native boolean objc_msgSendSuper_bool(objc_super superId, long sel, long arg0, NSPoint arg1);
/** @method flags=cast */
public static final native long objc_msgSendSuper(objc_super superId, long sel);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSendSuper(objc_super superId, long sel, NSPoint arg0);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSendSuper(objc_super superId, long sel, NSRect arg0);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSendSuper(objc_super superId, long sel, NSRect arg0, long arg1);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSendSuper(objc_super superId, long sel, NSSize arg0);
/** @method flags=cast */
public static final native long objc_msgSendSuper(objc_super superId, long sel, boolean arg0);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native long objc_msgSendSuper(objc_super superId, long sel, boolean arg0, NSRect arg1);
/** @method flags=cast */
public static final native long objc_msgSendSuper(objc_super superId, long sel, long arg0);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native long objc_msgSendSuper(objc_super superId, long sel, long arg0, NSPoint arg1);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native long objc_msgSendSuper(objc_super superId, long sel, long arg0, NSPoint arg1, long arg2);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native long objc_msgSendSuper(objc_super superId, long sel, long arg0, NSRect arg1, long arg2);
/** @method flags=cast */
public static final native long objc_msgSendSuper(objc_super superId, long sel, long arg0, boolean arg1);
/** @method flags=cast */
public static final native long objc_msgSendSuper(objc_super superId, long sel, long arg0, long arg1);
/** @method flags=cast */
public static final native long objc_msgSendSuper(objc_super superId, long sel, long arg0, long arg1, long arg2, boolean arg3);
/** @method flags=cast */
public static final native long objc_msgSendSuper(objc_super superId, long sel, long arg0, long arg1, long arg2, long arg3);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSendSuper_stret(NSRect result, objc_super superId, long sel, NSRect arg0);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSendSuper_stret(NSRect result, objc_super superId, long sel, NSRect arg0, long arg1);
/** @method flags=cast */
public static final native void objc_msgSendSuper_stret(NSRect result, objc_super superId, long sel, long arg0);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native void objc_msgSendSuper_stret(NSRect result, objc_super superId, long sel, long arg0, NSRect arg1, long arg2);
/** @method flags=cast */
public static final native void objc_msgSendSuper_stret(NSSize result, objc_super superId, long sel);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSendSuper_stret(NSSize result, objc_super superId, long sel, NSRect arg0);
/** @method flags=cast */
public static final native void objc_msgSendSuper_stret(NSSize result, objc_super superId, long sel, boolean arg0);

/** Sends */

/** @method flags=cast */
public static final native boolean objc_msgSend_bool(long id, long sel);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native boolean objc_msgSend_bool(long id, long sel, NSPoint arg0);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 * @param arg1 flags=struct
 */
public static final native boolean objc_msgSend_bool(long id, long sel, NSPoint arg0, NSRect arg1);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native boolean objc_msgSend_bool(long id, long sel, NSRange arg0, long arg1);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native boolean objc_msgSend_bool(long id, long sel, NSRect arg0);
/** @method flags=cast */
public static final native boolean objc_msgSend_bool(long id, long sel, long arg0);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native boolean objc_msgSend_bool(long id, long sel, long arg0, NSPoint arg1);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native boolean objc_msgSend_bool(long id, long sel, long arg0, NSSize arg1, boolean arg2);
/** @method flags=cast */
public static final native boolean objc_msgSend_bool(long id, long sel, long arg0, long arg1);
/** @method flags=cast */
public static final native boolean objc_msgSend_bool(long id, long sel, long arg0, long arg1, long arg2);
/** @method flags=cast */
public static final native boolean objc_msgSend_bool(long id, long sel, long arg0, long arg1, long arg2, long arg3);
/** @method flags=cast */
public static final native boolean objc_msgSend_bool(long id, long sel, long arg0, long arg1, long arg2, long arg3, long arg4);
/** @method flags=cast */
public static final native double objc_msgSend_fpret(long id, long sel);
/** @method flags=cast */
public static final native double objc_msgSend_fpret(long id, long sel, long arg0);
/** @method flags=cast */
public static final native double objc_msgSend_fpret(long id, long sel, long arg0, long arg1);
/** @method flags=cast */
public static final native float objc_msgSend_floatret(long id, long sel);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSAffineTransformStruct arg0);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSPoint arg0);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 * @param arg1 flags=struct
 * @param arg2 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSPoint arg0, NSPoint arg1, NSPoint arg2);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSPoint arg0, NSPoint arg1, long arg2);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSPoint arg0, double arg1, double arg2, double arg3, boolean arg4);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSPoint arg0, long arg1);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSPoint arg0, long arg1, double[] arg2);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRange arg0);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRange arg0, NSPoint arg1);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRange arg0, NSRange arg1, long arg2, long[] arg3);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRange arg0, long arg1);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRange arg0, long arg1, long arg2, long arg3, long arg4, byte[] arg5);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRect arg0);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRect arg0, NSPoint arg1);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRect arg0, NSRect arg1, long arg2, double arg3);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRect arg0, NSSize arg1);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRect arg0, boolean arg1);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRect arg0, boolean arg1, boolean arg2);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRect arg0, double arg1);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRect arg0, double arg1, double arg2);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRect arg0, long arg1);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRect arg0, long arg1, boolean arg2, long arg3);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRect arg0, long arg1, long arg2);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRect arg0, long arg1, long arg2, boolean arg3);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSRect arg0, long arg1, long arg2, boolean arg3, long arg4);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, NSSize arg0);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, boolean arg0);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, boolean arg0, NSRect arg1);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, boolean arg0, long arg1);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, byte[] arg0);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, byte[] arg0, long arg1);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, char[] arg0);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, char[] arg0, NSRange arg1);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, char[] arg0, long arg1);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, double arg0);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, double arg0, double arg1);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, double arg0, double arg1, double arg2, double arg3);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, double arg0, long arg1, long arg2, long arg3, boolean arg4);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, double[] arg0);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, double[] arg0, long arg1, double arg2);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, int arg0);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, int[] arg0);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, int[] arg0, long arg1);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long arg0);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, long arg0, NSPoint arg1);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 * @param arg2 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, long arg0, NSPoint arg1, NSSize arg2, long arg3, long arg4, long arg5, boolean arg6);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, long arg0, NSPoint arg1, long arg2);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, long arg0, NSPoint arg1, long arg2, double arg3, long arg4, long arg5, long arg6, long arg7, long arg8);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, long arg0, NSPoint arg1, long arg2, double arg3, long arg4, long arg5, short arg6, long arg7, long arg8);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, long arg0, NSRange arg1);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, long arg0, NSRect arg1, long arg2);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long arg0, boolean arg1);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long arg0, double arg1);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long arg0, long arg1);
/**
 * @method flags=cast
 * @param arg2 flags=struct
 */
public static final native long objc_msgSend(long id, long sel, long arg0, long arg1, NSRange arg2);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long arg0, long arg1, boolean arg2);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long arg0, long arg1, double arg2, long arg3);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long arg0, long arg1, long arg2);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long arg0, long arg1, long arg2, boolean arg3);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long arg0, long arg1, long arg2, long arg3);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long arg0, long arg1, long arg2, long arg3, long arg4);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long arg0, long arg1, long arg2, long arg3, long arg4, boolean arg5, boolean arg6, long arg7, long arg8, long arg9, long arg10);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long arg0, long arg1, long arg2, long arg3, long arg4, long arg5);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long arg0, long arg1, long arg2, long arg3, long arg4, long arg5, long arg6);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long[] arg0, int arg1, int arg2);
/** @method flags=cast */
public static final native long objc_msgSend(long id, long sel, long[] arg0, long arg1, long arg2);
/** @method flags=cast */
public static final native void objc_msgSend_stret(NSAffineTransformStruct result, long id, long sel);
/** @method flags=cast */
public static final native void objc_msgSend_stret(NSOperatingSystemVersion result, long id, long sel);
/** @method flags=cast */
public static final native void objc_msgSend_stret(NSPoint result, long id, long sel);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSend_stret(NSPoint result, long id, long sel, NSPoint arg0);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSend_stret(NSPoint result, long id, long sel, NSPoint arg0, long arg1);
/** @method flags=cast */
public static final native void objc_msgSend_stret(NSPoint result, long id, long sel, long arg0);
/** @method flags=cast */
public static final native void objc_msgSend_stret(NSRange result, long id, long sel);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSend_stret(NSRange result, long id, long sel, NSRange arg0, long arg1);
/** @method flags=cast */
public static final native void objc_msgSend_stret(NSRange result, long id, long sel, long arg0);
/** @method flags=cast */
public static final native void objc_msgSend_stret(NSRect result, long id, long sel);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSend_stret(NSRect result, long id, long sel, NSRange arg0, long arg1);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSend_stret(NSRect result, long id, long sel, NSRect arg0);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSend_stret(NSRect result, long id, long sel, NSRect arg0, long arg1);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSend_stret(NSRect result, long id, long sel, NSSize arg0, long arg1);
/** @method flags=cast */
public static final native void objc_msgSend_stret(NSRect result, long id, long sel, long arg0);
/**
 * @method flags=cast
 * @param arg1 flags=struct
 */
public static final native void objc_msgSend_stret(NSRect result, long id, long sel, long arg0, NSRect arg1, long arg2);
/** @method flags=cast */
public static final native void objc_msgSend_stret(NSRect result, long id, long sel, long arg0, long arg1);
/** @method flags=cast */
public static final native void objc_msgSend_stret(NSRect result, long id, long sel, long arg0, long arg1, boolean arg2);
/** @method flags=cast */
public static final native void objc_msgSend_stret(NSSize result, long id, long sel);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSend_stret(NSSize result, long id, long sel, NSRect arg0);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSend_stret(NSSize result, long id, long sel, NSSize arg0);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSend_stret(NSSize result, long id, long sel, NSSize arg0, boolean arg1, boolean arg2, long arg3);
/**
 * @method flags=cast
 * @param arg0 flags=struct
 */
public static final native void objc_msgSend_stret(NSSize result, long id, long sel, NSSize arg0, long arg1, long arg2, long arg3, long arg4, long arg5);
/** @method flags=cast */
public static final native void objc_msgSend_stret(NSSize result, long id, long sel, boolean arg0);

/** Sizeof natives */
public static final native int CGAffineTransform_sizeof();
public static final native int CGPathElement_sizeof();
public static final native int CGPoint_sizeof();
public static final native int CGRect_sizeof();
public static final native int CGSize_sizeof();
public static final native int NSAffineTransformStruct_sizeof();
public static final native int NSOperatingSystemVersion_sizeof();
public static final native int NSPoint_sizeof();
public static final native int NSRange_sizeof();
public static final native int NSRect_sizeof();
public static final native int NSSize_sizeof();

/** Memmove natives */

/**
 * @param dest cast=(void *),flags=no_in critical
 * @param src cast=(void *),flags=critical
 */
public static final native void memmove(long dest, CGPathElement src, long size);
/**
 * @param dest cast=(void *),flags=no_in critical
 * @param src cast=(void *),flags=critical
 */
public static final native void memmove(CGPathElement dest, long src, long size);
/**
 * @param dest cast=(void *),flags=no_in critical
 * @param src cast=(void *),flags=critical
 */
public static final native void memmove(long dest, NSPoint src, long size);
/**
 * @param dest cast=(void *),flags=no_in critical
 * @param src cast=(void *),flags=critical
 */
public static final native void memmove(NSPoint dest, long src, long size);
/**
 * @param dest cast=(void *),flags=no_in critical
 * @param src cast=(void *),flags=critical
 */
public static final native void memmove(long dest, NSRange src, long size);
/**
 * @param dest cast=(void *),flags=no_in critical
 * @param src cast=(void *),flags=critical
 */
public static final native void memmove(NSRange dest, long src, long size);
/**
 * @param dest cast=(void *),flags=no_in critical
 * @param src cast=(void *),flags=critical
 */
public static final native void memmove(long dest, NSRect src, long size);
/**
 * @param dest cast=(void *),flags=no_in critical
 * @param src cast=(void *),flags=critical
 */
public static final native void memmove(NSRect dest, long src, long size);
/**
 * @param dest cast=(void *),flags=no_in critical
 * @param src cast=(void *),flags=critical
 */
public static final native void memmove(long dest, NSSize src, long size);
/**
 * @param dest cast=(void *),flags=no_in critical
 * @param src cast=(void *),flags=critical
 */
public static final native void memmove(NSSize dest, long src, long size);

/** This section is auto generated */

static {
	NSOperatingSystemVersion version = NSProcessInfo.processInfo().operatingSystemVersion();
	VERSION = VERSION ((int)version.majorVersion, (int)version.minorVersion, (int)version.patchVersion);
}

}
