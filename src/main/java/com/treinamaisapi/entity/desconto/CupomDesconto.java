package com.treinamaisapi.entity.desconto;

import com.treinamaisapi.entity.enums.desconto.TipoDesconto;
import com.treinamaisapi.entity.pacotes.Pacote;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "CUPONS_DESCONTO")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CupomDesconto {

    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, unique = true, length = 40)
    private String codigo; // ex: "PETRO10"

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private TipoDesconto tipo; // PERCENTUAL ou VALOR_FIXO

    @Column(nullable = false, precision = 10, scale = 2)
    private BigDecimal valor; // 10 (%) ou 15.00 (R$)

    @Column(nullable = false)
    private boolean ativo = true;

    private LocalDateTime inicioVigencia;
    private LocalDateTime fimVigencia;

    // limites
    private Integer limiteUsosTotal;     // null = ilimitado
    private Integer limiteUsosPorUsuario; // null = ilimitado

    // mínimo para aplicar
    @Column(precision = 10, scale = 2)
    private BigDecimal valorMinimoCompra;


}
