package com.treinamaisapi.common.dto.questao.response;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class QuestaoResponse {
    private Long id;
    private String enunciado;
    private String alternativaA;
    private String alternativaB;
    private String alternativaC;
    private String alternativaD;
    private String respostaCorreta;
    private String banca;
    private String nivelDificuldade;
    private String subcapitulo;
    private String capitulo;
    private String tema;
}

