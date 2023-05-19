package com.example.routes

import com.example.models.Customer
import com.example.models.customerStorage
import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.logging.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import java.io.StringWriter
import java.net.URLDecoder

fun Route.customerRouting() {
    route("/customer") {
        get("hi/{asdf?}") {
            val req = call.request
            val local = req.local
            val resp = "Customer stored correctly. \n" +
                    "accept: ${req.accept()} \n"
                    // "acceptCharset: ${req.acceptCharset()} \n" +
                    // "acceptCharsetItems: ${req.acceptCharsetItems()} \n" +
                    // "acceptEncoding: ${req.acceptEncoding()} \n" +
                    // "acceptEncodingItems: ${req.acceptEncodingItems()} \n" +
                    // "acceptItems: ${req.acceptItems()} \n" +
                    // "acceptLanguage: ${req.acceptLanguage()} \n" +
                    // "acceptLanguageItems: ${req.acceptLanguageItems()} \n" +
                    // "cacheControl: ${req.cacheControl()} \n" +
                    // "cookies: ${req.cookies["asdf"]} \n" +
                    // "rawCookies: ${req.cookies.rawCookies["asdf"]} \n" +
                    // "document (raw): ${req.document()} \n" +
                    // "document (decoded): ${URLDecoder.decode(req.document())} \n" +
                    // "header (asdf): ${req.header("asdf")} \n" +
//                        "host: ${req.host()} \n" +
                    // "location: ${req.location()} \n" +
                    // "local.localAddress: ${local.localAddress} \n" +
                    // "local.localHost: ${local.localHost} \n" +
                    // "local.remoteAddress: ${local.remoteAddress} \n" +
                    // "local.remoteHost: ${local.remoteHost} \n" +
                    // "local.serverHost: ${local.serverHost} \n" +
                    // "local.uri: ${local.uri} \n" +
                    // "path (raw): ${req.path()} \n" +
                    // "path (decoded): ${URLDecoder.decode(req.path())} \n" +
                    // "queryString (raw): ${req.queryString()} \n" +
                    // "queryString (decoded): ${URLDecoder.decode(req.queryString())} \n" +
                    // "queryParameters: ${req.queryParameters["hi"]} \n" +
                    // "rawQueryParameters (raw): ${req.rawQueryParameters["hi"]} \n" +
                    // "rawQueryParameters (decoded): ${URLDecoder.decode(req.rawQueryParameters["hi"])} \n" +
                    // "toLogString: ${req.toLogString()} \n" +
                    // "uri: ${req.uri} \n" +
                    // "userAgent (raw): ${req.userAgent()} \n" +
                    // "userAgent (decoded): ${URLDecoder.decode(req.userAgent())} \n"

            call.respondBytes(
                // ruleid: ktor_request_xss_2
                bytes="hi! ${resp}",
                contentType = ContentType.Text.Html,
            )
        }
    }
}
