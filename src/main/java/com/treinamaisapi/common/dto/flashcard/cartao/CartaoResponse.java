package com.treinamaisapi.common.dto.flashcard.cartao;

public record CartaoResponse(
        Long id,
        String frente,
        String verso,
        boolean precisaRevisar
) {}

