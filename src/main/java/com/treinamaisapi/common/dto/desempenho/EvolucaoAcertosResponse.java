package com.treinamaisapi.common.dto.desempenho;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class EvolucaoAcertosResponse {
    private String data;
    private Double percentual;
}
