package com.treinamaisapi.common.dto.auth.resetSenha;

public record ConfirmarCodigoRequest(
        String email,
        String codigo
) {}
