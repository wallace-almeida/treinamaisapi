package com.treinamaisapi.common.dto.simulado.request;

import lombok.Data;

@Data
public class RespostaQuestaoSimulado {
    private Long questaoId;
    private String respostaUsuario; // Ex: "A", "B", "C", "D"
}
