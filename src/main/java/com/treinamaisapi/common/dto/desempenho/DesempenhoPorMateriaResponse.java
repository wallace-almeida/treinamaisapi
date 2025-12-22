package com.treinamaisapi.common.dto.desempenho;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class DesempenhoPorMateriaResponse {
    private String materia;
    private Double percentual;
}
