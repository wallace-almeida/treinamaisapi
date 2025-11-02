package com.treinamaisapi.common.dto.simulado.request;

import lombok.Data;

@Data
public class CriarSimuladoRequest {
    private Long concursoId;
    private Long temaId;
    private Long capituloId;
    private Long subcapituloId;
    private String nivelDificuldade; // FACIL, MEDIO, DIFICIL
    private String banca; // ex: CESPE, FGV, Plataforma
    private Integer quantidadeQuestoes;
    private Integer tempoDuracao;
}