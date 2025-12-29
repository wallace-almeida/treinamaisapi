package com.treinamaisapi.common.dto.flashcard.cartao;

public record CartaoRequest(
        String frente,
        String verso,
        Long temaId,
        Long baralhoId
) {}


