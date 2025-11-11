package com.treinamaisapi.entity.simulado;

import com.treinamaisapi.entity.enums.StatusSimulado;
import com.treinamaisapi.entity.questoes_respondida.QuestaoSimulado;
import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "SIMULADOS")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Simulado {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String titulo;

    @ManyToOne
    @JoinColumn(name = "usuario_id", nullable = false)
    private Usuario usuario;

    private Integer quantidadeQuestoes;
    private Integer tempoDuracao;

    private LocalDateTime dataCriacao;
    private LocalDateTime dataFinalizacao;

    @Enumerated(EnumType.STRING)
    private StatusSimulado status;

    private Double pontuacaoFinal;

    // Filtros usados
    @ElementCollection
    private List<Long> temaIds;

    @ElementCollection
    private List<Long> capituloIds;

    @ElementCollection
    private List<Long> subcapituloIds;

    @ElementCollection
    private List<String> bancas;

    @ElementCollection
    private List<String> niveis;

    // Nova: perfil inteligente
    private Boolean inteligente;
    private Boolean balanceado;
    private Boolean prioridadeFraquezas;

}