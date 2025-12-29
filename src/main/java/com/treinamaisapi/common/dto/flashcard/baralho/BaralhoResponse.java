package com.treinamaisapi.common.dto.flashcard.baralho;

public record BaralhoResponse(
        Long id,
        String titulo,
        String temaNome,
        int quantidadeCartoes
) {}

