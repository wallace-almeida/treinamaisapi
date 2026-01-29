package com.treinamaisapi.common.dto.simuladoSugestao;

import lombok.Builder;

@Builder
public record TemaDesempenhoDTO(
        Long temaId,
        String temaNome,
        Long totalRespondidas,
        Long totalErros,
        Double taxaAcerto
) {
}
