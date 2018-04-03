package com.ml.exceptions;


abstract class AbstractApiRestException extends RuntimeException{

    protected AbstractApiRestException(String message){
        super(message);
    }

}
