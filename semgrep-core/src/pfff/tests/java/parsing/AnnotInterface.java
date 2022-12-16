package com.facebook.orca.annotations;

import java.lang.annotation.Retention;

import com.google.inject.BindingAnnotation;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * Binding annotation to indicate the production platform app http config.
 */
@BindingAnnotation
@Retention(RUNTIME)
public @interface ProductionPlatformAppHttpConfig {
}
