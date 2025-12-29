package com.treinamaisapi.common.dto.flashcard.cartao;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FlashcardEstudoResponse {
    private Long cartaoId;
    private String frente;
    private String verso;

    private Integer pendentesHoje; // total ainda faltando
    private Integer revisadosHoje; // total já feito
    private Integer metaDiariaPercentual;
}


