package com.treinamaisapi.common.dto.simulado.response;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class QuestaoResumoResponse {
    private Long id;
    private String enunciado;
    private String nivelDificuldade;
    private String banca;
}
