package com.treinamaisapi.common.dto.auth.resetSenha;

public record RedefinirSenhaRequest(
        String token,
        String novaSenha
) {}
