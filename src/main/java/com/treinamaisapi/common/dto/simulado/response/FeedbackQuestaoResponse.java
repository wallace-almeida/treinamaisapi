package com.treinamaisapi.common.dto.simulado.response;

import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class FeedbackQuestaoResponse {

    private Long questaoId;
    private String enunciado;
    private String respostaCorreta;
    private String respostaUsuario;
    private Boolean correta;
    private String explicacao;
}
