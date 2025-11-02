package com.treinamaisapi.common.dto.compra.response;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class CompraResponse {
    private Long id;
    private String usuario;
    private String pacote;
    private Boolean ativo;
    private LocalDateTime dataCompra;
    private LocalDateTime dataExpiracao;
}
