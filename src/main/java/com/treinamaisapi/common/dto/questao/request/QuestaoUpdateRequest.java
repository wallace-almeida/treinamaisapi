package com.treinamaisapi.common.dto.questao.request;

import com.treinamaisapi.entity.enums.NivelDificuldade;
import lombok.Data;

@Data
public class QuestaoUpdateRequest {
    private String enunciado;
    private String alternativaA;
    private String alternativaB;
    private String alternativaC;
    private String alternativaD;
    private String respostaCorreta;   // "A"/"B"/"C"/"D"
    private String explicacao;
    private NivelDificuldade nivelDificuldade;
    private String banca;
    private Long subcapituloId;       // se quiser permitir trocar o subcapítulo
}
