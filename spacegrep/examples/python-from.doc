/* *
 *
 *  Copyright (c) 2019-2020 Highsoft AS
 *
 *  Boost module: stripped-down renderer for higher performance
 *
 *  License: highcharts.com/license
 *
 *  !!!!!!! SOURCE GETS TRANSPILED BY TYPESCRIPT. EDIT TS FILE ONLY. !!!!!!!
 *
 * */
'use strict';
/* eslint-disable valid-jsdoc */
/**
 * Vertex Buffer abstraction.
 * A vertex buffer is a set of vertices which are passed to the GPU
 * in a single call.
 *
 * @private
 * @function GLVertexBuffer
 *
 * @param {WebGLContext} gl
 *        the context in which to create the buffer
 *
 * @param {GLShader} shader
 *        the shader to use
 *
 * @return {*}
 */
function GLVertexBuffer(gl, shader, dataComponents
/* , type */
) {
    var buffer = false, vertAttribute = false, components = dataComponents || 2, preAllocated = false, iterator = 0, 
    // farray = false,
    data;
    // type = type || 'float';
    /**
     * @private
     */
    function destroy() {
        if (buffer) {
            gl.deleteBuffer(buffer);
            buffer = false;
            vertAttribute = false;
        }
        iterator = 0;
        components = dataComponents || 2;
        data = [];
    }
    /**
     * Build the buffer
     * @private
     * @param dataIn {Array<float>} - a 0 padded array of indices
     * @param attrib {String} - the name of the Attribute to bind the buffer to
     * @param dataComponents {Integer} - the number of components per. indice
     */
    function build(dataIn, attrib, dataComponents) {
        var farray;
        data = dataIn || [];
        if ((!data || data.length === 0) && !preAllocated) {
            // console.error('trying to render empty vbuffer');
            destroy();
            return false;
        }
        components = dataComponents || components;
        if (buffer) {
            gl.deleteBuffer(buffer);
        }
        if (!preAllocated) {
            farray = new Float32Array(data);
        }
        buffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
        gl.bufferData(gl.ARRAY_BUFFER, preAllocated || farray, gl.STATIC_DRAW);
        // gl.bindAttribLocation(shader.program(), 0, 'aVertexPosition');
        vertAttribute = gl.getAttribLocation(shader.program(), attrib);
        gl.enableVertexAttribArray(vertAttribute);
        // Trigger cleanup
        farray = false;
        return true;
    }
    /**
     * Bind the buffer
     * @private
     */
    function bind() {
        if (!buffer) {
            return false;
        }
        // gl.bindAttribLocation(shader.program(), 0, 'aVertexPosition');
        // gl.enableVertexAttribArray(vertAttribute);
        // gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
        gl.vertexAttribPointer(vertAttribute, components, gl.FLOAT, false, 0, 0);
        // gl.enableVertexAttribArray(vertAttribute);
    }
    /**
     * Render the buffer
     * @private
     * @param from {Integer} - the start indice
     * @param to {Integer} - the end indice
     * @param drawMode {String} - the draw mode
     */
    function render(from, to, drawMode) {
        var length = preAllocated ? preAllocated.length : data.length;
        if (!buffer) {
            return false;
        }
        if (!length) {
            return false;
        }
        if (!from || from > length || from < 0) {
            from = 0;
        }
        if (!to || to > length) {
            to = length;
        }
        drawMode = drawMode || 'points';
        gl.drawArrays(gl[drawMode.toUpperCase()], from / components, (to - from) / components);
        return true;
    }
    /**
     * @private
     */
    function push(x, y, a, b) {
        if (preAllocated) { // && iterator <= preAllocated.length - 4) {
            preAllocated[++iterator] = x;
            preAllocated[++iterator] = y;
            preAllocated[++iterator] = a;
            preAllocated[++iterator] = b;
        }
    }
    /**
     * Note about pre-allocated buffers:
     *     - This is slower for charts with many series
     * @private
     */
    function allocate(size) {
        size *= 4;
        iterator = -1;
        preAllocated = new Float32Array(size);
    }
    // /////////////////////////////////////////////////////////////////////////
    return {
        destroy: destroy,
        bind: bind,
        data: data,
        build: build,
        render: render,
        allocate: allocate,
        push: push
    };
}
export default GLVertexBuffer;
