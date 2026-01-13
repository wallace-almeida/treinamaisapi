package com.treinamaisapi.common.dto.concurso.response;


import com.treinamaisapi.entity.enums.concursos.StatusConcurso;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ConcursoStatusResponse {
    private Long id;
    private String nome;
    private StatusConcurso status;
}
