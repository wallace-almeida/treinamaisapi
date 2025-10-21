package com.treinamaisapi.common.dto.questao.request;

import com.treinamaisapi.entity.enums.NivelDificuldade;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class QuestaoRequest {
    @NotBlank
    private String enunciado;
    @NotBlank
    private String alternativaA;
    @NotBlank
    private String alternativaB;
    @NotBlank
    private String alternativaC;
    @NotBlank
    private String alternativaD;
    @NotBlank
    private String respostaCorreta;
    @NotNull
    private NivelDificuldade nivelDificuldade;
    private String banca;
    @NotNull
    private Long subcapituloId;
}
