/**
 *    Copyright (C) 2018-present MongoDB, Inc.
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the Server Side Public License, version 1,
 *    as published by MongoDB, Inc.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    Server Side Public License for more details.
 *
 *    You should have received a copy of the Server Side Public License
 *    along with this program. If not, see
 *    <http://www.mongodb.com/licensing/server-side-public-license>.
 *
 *    As a special exception, the copyright holders give permission to link the
 *    code of portions of this program with the OpenSSL library under certain
 *    conditions as described in each individual source file and distribute
 *    linked combinations including the program with the OpenSSL library. You
 *    must comply with the Server Side Public License in all respects for
 *    all of the code used other than as permitted herein. If you modify file(s)
 *    with this exception, you may extend this exception to your version of the
 *    file(s), but you are not obligated to do so. If you do not wish to do so,
 *    delete this exception statement from your version. If you delete this
 *    exception statement from all source files in the program, then also delete
 *    it in the license file.
 */

#include "mongo/util/net/cidr.h"

#include "mongo/bson/bsonobjbuilder.h"
#include "mongo/platform/basic.h"

#ifdef _WIN32
#include <Ws2tcpip.h>
#else
#include <arpa/inet.h>
#include <netinet/in.h>
#endif

using std::begin;
using std::end;
using std::find;

namespace mongo {

namespace {

constexpr std::uint8_t kIPv4Bits = 32;
constexpr std::uint8_t kIPv6Bits = 128;

#ifdef _WIN32
/**
 * Windows doesn't declare the second arg as const, but it is.
 * Also, it should blindly downcast to void*, but it doesn't.
 */
const char* inet_ntop(INT af, const std::uint8_t* addr, PSTR buf, std::size_t bufsz) {
    return ::inet_ntop(af, const_cast<void*>(reinterpret_cast<const void*>(addr)), buf, bufsz);
}
#endif

/**
 * `std::stoi()` naturally throws `std::invalid_argument` if str
 * isn't numeric at all or `std::out_of_range` if str won't fit in an int.
 *
 * Extend that to include a check that the string is entirely numeric and throw
 * `std::invalid_argument` for that as well
 */
int strict_stoi(const std::string& str, int base = 10) {
    std::size_t pos;
    auto len = std::stoi(str, &pos, base);
    if (pos != str.size()) {
        throw std::invalid_argument("Invalid characters encountered parsing: " + str + " at " +
                                    str.substr(pos));
    }
    return len;
}

template <class T>
T& append(T& s, int family, const std::array<uint8_t, 16> ip, int len) {
    char buffer[INET6_ADDRSTRLEN + 1] = {};
    if (inet_ntop(family, ip.data(), buffer, sizeof(buffer) - 1)) {
        s << buffer << '/' << (int)len;
    }
    return s;
}

}  // namespace

StatusWith<CIDR> CIDR::parse(BSONElement from) noexcept {
    if (from.type() != String) {
        return {ErrorCodes::UnsupportedFormat, "CIDR range must be a string"};
    }
    return parse(from.valueStringData());
}

StatusWith<CIDR> CIDR::parse(StringData s) noexcept try {
    return CIDR(s);
} catch (const DBException& e) {
    return e.toStatus();
}

CIDR::CIDR(StringData s) try {
    _ip.fill(0);
    auto slash = find(begin(s), end(s), '/');
    auto ip = (slash == end(s)) ? s.toString() : s.substr(0, slash - begin(s)).toString();

    if (inet_pton(AF_INET, ip.c_str(), _ip.data())) {
        _family = AF_INET;
        _len = kIPv4Bits;
    } else if (inet_pton(AF_INET6, ip.c_str(), _ip.data())) {
        _family = AF_INET6;
        _len = kIPv6Bits;
    } else {
        uasserted(ErrorCodes::UnsupportedFormat, "Invalid IP address in CIDR string");
    }

    if (slash == end(s)) {
        return;
    }

    auto len = strict_stoi(std::string(slash + 1, end(s)), 10);
    uassert(ErrorCodes::UnsupportedFormat,
            "Invalid length in CIDR string",
            (len >= 0) && (len <= _len));
    _len = len;

} catch (const std::invalid_argument&) {
    uasserted(ErrorCodes::UnsupportedFormat, "Non-numeric length in CIDR string");
} catch (const std::out_of_range&) {
    uasserted(ErrorCodes::UnsupportedFormat, "Invalid length in CIDR string");
}

template <>
BSONObjBuilder& BSONObjBuilderValueStream::operator<<<CIDR>(CIDR value) {
    _builder->append(_fieldName, value.toString());
    _fieldName = StringData();
    return *_builder;
}

}  // namespace mongo

std::ostream& mongo::operator<<(std::ostream& s, const CIDR& cidr) {
    return append(s, cidr._family, cidr._ip, cidr._len);
}

mongo::StringBuilder& mongo::operator<<(StringBuilder& s, const CIDR& cidr) {
    return append(s, cidr._family, cidr._ip, cidr._len);
}
