package com.treinamaisapi.entity.historico_estudo;

import com.treinamaisapi.entity.enums.TipoAtividade;
import com.treinamaisapi.entity.simulado.Simulado;
import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;

import java.time.LocalDateTime;

@Entity
@Table(name = "HISTORICO_ESTUDO")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class HistoricoEstudo {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Enumerated(EnumType.STRING)
    private TipoAtividade tipoAtividade; // SIMULADO, REVISAO, FLASHCARD

    private Double pontuacaoObtida;

    private Integer acertos;

    @CreationTimestamp
    private LocalDateTime dataEstudo;

    @ManyToOne
    @JoinColumn(name = "usuario_id")
    private Usuario usuario;

    @ManyToOne
    @JoinColumn(name = "simulado_id", nullable = true)
    private Simulado simulado;

}
