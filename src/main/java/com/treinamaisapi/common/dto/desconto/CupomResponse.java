package com.treinamaisapi.common.dto.desconto;

import com.treinamaisapi.entity.enums.desconto.TipoDesconto;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@Builder
public class CupomResponse {
    private Long id;
    private String codigo;
    private TipoDesconto tipo;
    private BigDecimal valor;
    private boolean ativo;
    private LocalDateTime inicioVigencia;
    private LocalDateTime fimVigencia;
    private Integer limiteUsosTotal;
    private Integer limiteUsosPorUsuario;
    private BigDecimal valorMinimoCompra;
}
