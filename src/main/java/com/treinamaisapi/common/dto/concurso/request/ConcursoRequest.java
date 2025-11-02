package com.treinamaisapi.common.dto.concurso.request;

import lombok.Data;

import java.time.LocalDate;

@Data
public class ConcursoRequest {
    private String nome;
    private String descricao;
    private LocalDate dataProva;
}
