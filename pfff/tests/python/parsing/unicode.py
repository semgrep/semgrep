# from https://raw.githubusercontent.com/uber-archive/pyflame/v1.6.7/tests/sleeper_%E3%83%A6%E3%83%8B%E3%82%B3%E3%83%BC%E3%83%89.py
# Copyright 2017 Uber Technologies, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import os
import sys
import time


def _sleep(sleep_time):
    time.sleep(sleep_time)
    target = time.time() + sleep_time
    while time.time() < target:
        pass


def låtìÑ1(sleep_time):
    _sleep(sleep_time)


# Arabic
def وظيفة(sleep_time):
    _sleep(sleep_time)


# Japanese
def 日本語はどうですか(sleep_time):
    _sleep(sleep_time)


# Khmer
def មុខងារ(sleep_time):
    _sleep(sleep_time)


# Thai
def ฟังก์ชัน(sleep_time):
    _sleep(sleep_time)


def main():
    sys.stdout.write('%d\n' % (os.getpid(), ))
    sys.stdout.flush()
    while True:
        låtìÑ1(0.1)
        وظيفة(0.1)
        日本語はどうですか(0.1)
        មុខងារ(0.1)
        ฟังก์ชัน(0.1)


if __name__ == '__main__':
    main()
