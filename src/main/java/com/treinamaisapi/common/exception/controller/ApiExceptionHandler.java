package com.treinamaisapi.common.exception.controller;

import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.common.exception.ErrorResponse;
import com.treinamaisapi.common.exception.NotFoundException;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.UUID;

@RestControllerAdvice
public class ApiExceptionHandler {

    private static final Logger log = LoggerFactory.getLogger(ApiExceptionHandler.class);

    private String getTraceId() {
        return UUID.randomUUID().toString();
    }

    @ExceptionHandler(NotFoundException.class)
    public ResponseEntity<ErrorResponse> handleNotFound(NotFoundException ex, HttpServletRequest request) {

        var traceId = getTraceId();
        log.warn("NOT_FOUND [{}]: {}", traceId, ex.getMessage());

        var error = ErrorResponse.of(
                HttpStatus.NOT_FOUND.value(),
                "NOT_FOUND",
                ex.getMessage(),
                request.getRequestURI(),
                traceId
        );

        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
    }

    @ExceptionHandler(BusinessException.class)
    public ResponseEntity<ErrorResponse> handleBusiness(BusinessException ex, HttpServletRequest request) {

        var traceId = getTraceId();
        log.warn("BUSINESS_ERROR [{}]: {}", traceId, ex.getMessage());

        var error = ErrorResponse.of(
                HttpStatus.BAD_REQUEST.value(),
                "BUSINESS_ERROR",
                ex.getMessage(),
                request.getRequestURI(),
                traceId
        );

        return ResponseEntity.badRequest().body(error);
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ErrorResponse> handleValidation(MethodArgumentNotValidException ex, HttpServletRequest request) {

        var traceId = getTraceId();

        var message = ex.getBindingResult().getFieldErrors()
                .stream()
                .map(e -> e.getField() + ": " + e.getDefaultMessage())
                .findFirst()
                .orElse("Erro de validação");

        log.warn("VALIDATION_ERROR [{}]: {}", traceId, message);

        var error = ErrorResponse.of(
                HttpStatus.UNPROCESSABLE_ENTITY.value(),
                "VALIDATION_ERROR",
                message,
                request.getRequestURI(),
                traceId
        );

        return ResponseEntity.unprocessableEntity().body(error);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> handleGeneric(Exception ex, HttpServletRequest request) {

        var traceId = getTraceId();
        log.error("INTERNAL_ERROR [{}]: {}", traceId, ex.getMessage(), ex);

        var error = ErrorResponse.of(
                HttpStatus.INTERNAL_SERVER_ERROR.value(),
                "INTERNAL_ERROR",
                "Ocorreu um erro inesperado.",
                request.getRequestURI(),
                traceId
        );

        return ResponseEntity.internalServerError().body(error);
    }
}
