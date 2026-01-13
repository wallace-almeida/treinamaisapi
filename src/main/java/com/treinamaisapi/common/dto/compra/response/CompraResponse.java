package com.treinamaisapi.common.dto.compra.response;

import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@Builder
public class CompraResponse {
    private Long id;

    private Long pacoteId;
    private String pacoteNome;

    private BigDecimal valor;

    private StatusCompra status;
    private Boolean ativo;

    private LocalDateTime dataCompra;
    private LocalDateTime dataExpiracao;
}
