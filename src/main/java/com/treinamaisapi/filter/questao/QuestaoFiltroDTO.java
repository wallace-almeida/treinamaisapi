package com.treinamaisapi.filter.questao;

public record QuestaoFiltroDTO(
        Long id,
        String enunciado,
        String alternativaA,
        String alternativaB,
        String alternativaC,
        String alternativaD,
        String respostaCorreta,
        String banca,
        String nivelDificuldade,
        Long subcapituloId
) {}
