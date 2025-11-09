package com.treinamaisapi.common.exception;

import java.time.LocalDateTime;
import java.util.Map;

public record ErrorResponse(
        LocalDateTime timestamp,
        int status,
        String error,
        String message,
        String path,
        String traceId
) {
    public static ErrorResponse of(
            int status,
            String error,
            String message,
            String path,
            String traceId
    ) {
        return new ErrorResponse(
                LocalDateTime.now(),
                status,
                error,
                message,
                path,
                traceId
        );
    }
}
