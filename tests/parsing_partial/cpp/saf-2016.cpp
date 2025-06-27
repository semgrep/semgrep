/***************************************************************************************************
 * Copyright (c) 2017 - 2025 NVIDIA CORPORATION & AFFILIATES. All rights reserved.
 * SPDX-License-Identifier: BSD-3-Clause
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 **************************************************************************************************/


// A snippet from: https://github.com/NVIDIA/cutlass/blob/3fe62887d8dd75700fdaf57f9c181878701b0802/include/cutlass/conv/conv2d_problem_size.h
// see saf-2016
// 
// at semgrep-proprietary/commit/3685b5ba79a242083c06d12649eb305597a608c8,
// the CPP parser would inject a self referential type to some of the variables
// present in this code file (i.e N, K ...). This is an issue, as during AST
// translation we just loop forever

struct Conv2dProblemSize {

  /// Constructs convolution problem size from cutlass Tensor4DCoord and MatrixCoord
  // computes output size and sets P and Q (skip output from ctor arguments)
  CUTLASS_HOST_DEVICE
  Conv2dProblemSize(
    cutlass::Tensor4DCoord input_size,   // NHWC
    cutlass::Tensor4DCoord filter_size,  // KRSC
    cutlass::Tensor4DCoord padding,      // pad_h, upper_pad_h, pad_w, upper_pad_w
    cutlass::MatrixCoord stride,         // stride_h, stride_w
    cutlass::MatrixCoord dilation,       // dilation_h, dilation_w
    cutlass::conv::Mode mode = cutlass::conv::Mode::kCrossCorrelation,
    int split_k_slices = 1,
    int groups = 1
  ):
    N(input_size.n()), H(input_size.h()), W(input_size.w()), C(input_size.c()),
    K(filter_size.n()), R(filter_size.h()), S(filter_size.w()),
    pad_h(padding[0]), pad_w(padding[2]),
    stride_h(stride.row()), stride_w(stride.column()),
    dilation_h(dilation.row()), dilation_w(dilation.column()),
    mode(mode), split_k_slices(split_k_slices), groups(groups) {
      // set output P and Q
      P = ((H + pad_h + padding[1] - R * dilation_h) / stride_h) + 1;
      Q = ((W + pad_w + padding[3] - S * dilation_w) / stride_w) + 1;
    }

  /// Constructs convolution problem size from cutlass Tensor4DCoord and MatrixCoord
  // set user-defined output size and sets P and Q (skip padding, striding, and dilation)
  CUTLASS_HOST_DEVICE
  Conv2dProblemSize(
    cutlass::Tensor4DCoord input_size,    // NHWC
    cutlass::Tensor4DCoord filter_size,   // KRSC
    cutlass::Tensor4DCoord output_size,   // NPQK
    cutlass::conv::Mode mode = cutlass::conv::Mode::kCrossCorrelation,
    int split_k_slices = 1,
    int groups = 1
  ):
    N(input_size.n()), H(input_size.h()), W(input_size.w()), C(input_size.c()),
    P(output_size.h()), Q(output_size.w()),
    K(filter_size.n()), R(filter_size.h()), S(filter_size.w()),
    pad_h(R / 2), pad_w(S / 2), stride_h(1), stride_w(1),
    dilation_h(1), dilation_w(1),
    mode(mode), split_k_slices(split_k_slices), groups(groups) {}


};

