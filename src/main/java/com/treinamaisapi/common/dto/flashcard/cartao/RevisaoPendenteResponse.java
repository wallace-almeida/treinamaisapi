package com.treinamaisapi.common.dto.flashcard.cartao;

import java.time.LocalDateTime;

public record RevisaoPendenteResponse(
        Long id,
        String frente,
        String verso,
        String tema,
        LocalDateTime ultimaRevisao
) {}

