package main

import (
	"context"
	"database/sql"
	"fmt"
	"http"
)

var db *sql.DB

func dbExec(r *http.Request) {
	customerId := r.URL.Query().Get("id")
	// ruleid: string-formatted-query
	query := "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = " + customerId

	row, _ := db.Exec(query)
}


func dbExecContext(r *http.Request) {
	ctx := context.Background()
	customerId := r.URL.Query().Get("id")
	// ruleid: string-formatted-query
	query := "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = " + customerId

	row, _ := db.ExecContext(ctx, query)
}


func dbQuery(r *http.Request) {
	customerId := r.URL.Query().Get("id")
	// ruleid: string-formatted-query
	query := "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = " + customerId

	row, _ := db.Query(query)
}


func dbQueryContext(r *http.Request) {
	ctx := context.Background()
	customerId := r.URL.Query().Get("id")
	// ruleid: string-formatted-query
	query := "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = " + customerId

	row, _ := db.QueryContext(ctx, query)
}


func dbQueryRow(r *http.Request) {
	customerId := r.URL.Query().Get("id")
	// ruleid: string-formatted-query
	query := "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = " + customerId

	row, _ := db.QueryRow(query)
}


func dbQueryRowContext(r *http.Request) {
	ctx := context.Background()
	customerId := r.URL.Query().Get("id")
	// ruleid: string-formatted-query
	query := "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = " + customerId

	row, _ := db.QueryRowContext(ctx, query)
}



func dbExecFmt(r *http.Request) {
	customerId := r.URL.Query().Get("id")
	// ruleid: string-formatted-query
	query := "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = %s"
    query = fmt.Printf(query, customerId)

	row, _ := db.Exec(query)
}


func dbExecContextFmt(r *http.Request) {
	ctx := context.Background()
	customerId := r.URL.Query().Get("id")
	// ruleid: string-formatted-query
	query := "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = %s"
    query = fmt.Printf(query, customerId)

	row, _ := db.ExecContext(ctx, query)
}


func dbQueryFmt(r *http.Request) {
	customerId := r.URL.Query().Get("id")
	// ruleid: string-formatted-query
	query := "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = %s"
    query = fmt.Printf(query, customerId)

	row, _ := db.Query(query)
}


func dbQueryContextFmt(r *http.Request) {
	ctx := context.Background()
	customerId := r.URL.Query().Get("id")
	// ruleid: string-formatted-query
	query := "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = %s"
    query = fmt.Printf(query, customerId)

	row, _ := db.QueryContext(ctx, query)
}


func dbQueryRowFmt(r *http.Request) {
	customerId := r.URL.Query().Get("id")
	// ruleid: string-formatted-query
	query := "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = %s"
    query = fmt.Printf(query, customerId)

	row, _ := db.QueryRow(query)
}


func dbQueryRowContextFmt(r *http.Request) {
	ctx := context.Background()
	customerId := r.URL.Query().Get("id")
	// ruleid: string-formatted-query
	query := "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = %s"
    query = fmt.Printf(query, customerId)

	row, _ := db.QueryRowContext(ctx, query)
}

func unmodifiedString() {
	// ok
	query := "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = 1234"
	row, _ := db.Query(query)
}

func unmodifiedStringDirectly() {
    // ok
	row, _ := db.Query("SELECT number, expireDate, cvv FROM creditcards WHERE customerId = 1234")
}

func badDirectQueryAdd(r *http.Request) {
    ctx := context.Background()
    customerId := r.URL.Query().Get("id")

	// ruleid: string-formatted-query
    row, _ := db.QueryRowContext(ctx, "SELECT number, expireDate, cvv FROM creditcards WHERE customerId = " + customerId)
}

func badDirectQueryFmt(r *http.Request) {
    ctx := context.Background()
    customerId := r.URL.Query().Get("id")

	// ruleid: string-formatted-query
    row, _ := db.QueryRowContext(ctx, fmt.Printf("SELECT number, expireDate, cvv FROM creditcards WHERE customerId = %s", customerId))
}

