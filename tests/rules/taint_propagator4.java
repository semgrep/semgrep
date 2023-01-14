package com.example.restservice;

import java.util.concurrent.atomic.AtomicLong;
import java.util.ArrayList;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletResponse;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.TypedQuery;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class GreetingController {
  
  @GetMapping("/greeting")
  public Greeting greeting(@RequestParam(value = "smth") String orderBy, HttpServletResponse response) {
    Page page = new Page("greeting");

    // NOTE: orderBy is the `source` here, it flows into Page object
    page.setOrderBy(orderBy);

    EntityManager em = HibernateOperations.getEntityManager();

    // NOTE: insecurely generated SQL string using tainted `orderBy`
    StringBuilder sqlBuilder = new StringBuilder();
    sqlBuilder.append("SELECT * FROM ( ");
    sqlBuilder.append(" SELECT TMP_PAGE.*, ROWNUM PAGEHELPER_ROW_ID FROM ( \n");
    sqlBuilder.append(page.getName());
    sqlBuilder.append("\n ) TMP_PAGE)");
    sqlBuilder.append(" WHERE PAGEHELPER_ROW_ID <= ? AND PAGEHELPER_ROW_ID > ?");
    sqlBuilder.append(" ORDER BY ");
    sqlBuilder.append(page.getOrderBy());
    // ruleid: jpa-sqli
    TypedQuery<Greeting> q = em.createQuery(sqlBuilder.toString(), Greeting.class);
    Greeting res = q.getSingleResult();
    return res;
  }

}

