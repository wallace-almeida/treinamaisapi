package com.treinamaisapi.common.dto.desconto;

import com.treinamaisapi.entity.enums.desconto.TipoDesconto;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
public class CriarCupomRequest {
    private String codigo;
    private TipoDesconto tipo;     // PERCENTUAL / VALOR_FIXO
    private BigDecimal valor;      // 10 (%), 15.00 (R$)
    private boolean ativo = true;

    private LocalDateTime inicioVigencia;
    private LocalDateTime fimVigencia;

    private Integer limiteUsosTotal;
    private Integer limiteUsosPorUsuario;

    private BigDecimal valorMinimoCompra;
}
