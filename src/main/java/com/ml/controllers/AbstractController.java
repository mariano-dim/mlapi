package com.ml.controllers;


import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import java.nio.charset.Charset;
import java.util.Collection;


public class AbstractController<T > {

    static final MediaType contentType = new MediaType(MediaType.APPLICATION_JSON.getType(),
            MediaType.APPLICATION_JSON.getSubtype(),
            Charset.forName("utf8"));

    protected ResponseEntity<T> createdResult(T result) {
        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.setLocation(ServletUriComponentsBuilder
                .fromCurrentRequest().path("/{id}")
                .buildAndExpand(result.getClass()).toUri());

        httpHeaders.setContentType(contentType);
        return new ResponseEntity<>(result, httpHeaders, HttpStatus.CREATED);
    }

    protected ResponseEntity<Collection<T>> collectionResult(Collection<T> result) {
        HttpHeaders httpHeaders = buildHeaders();
        return new ResponseEntity<>(result, httpHeaders, HttpStatus.OK);
    }

    protected ResponseEntity<T> createdErrorResult(T result) {
        HttpHeaders httpHeaders = buildHeaders();
        return new ResponseEntity<>(result, httpHeaders, HttpStatus.BAD_REQUEST);
    }

    protected ResponseEntity<T> singleResult(T result) {
        HttpHeaders httpHeaders = buildHeaders();
        return new ResponseEntity<>(result, httpHeaders, HttpStatus.OK);
    }

    private HttpHeaders buildHeaders() {
        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.setLocation(ServletUriComponentsBuilder
                .fromCurrentRequest().path("/")
                .build().toUri());
        httpHeaders.setContentType(contentType);
        return httpHeaders;
    }


}

