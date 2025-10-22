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
    private Integer tempoDuracao; // em minutos
    private LocalDateTime dataCriacao;

    @Enumerated(EnumType.STRING)
    private StatusSimulado status;

    private Double pontuacaoFinal;

    // filtros usados na criação
    private Long temaId;
    private Long capituloId;
    private Long subcapituloId;
    private String nivelDificuldade;
    private String banca;

    @OneToMany(mappedBy = "simulado", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<QuestaoSimulado> questoes;

}