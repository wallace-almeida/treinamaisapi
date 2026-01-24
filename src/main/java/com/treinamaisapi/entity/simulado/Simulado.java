package com.treinamaisapi.entity.simulado;

import com.treinamaisapi.entity.enums.StatusSimulado;
import com.treinamaisapi.entity.questoes_respondida.QuestaoSimulado;
import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

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


    @CreationTimestamp
    @Column(updatable = false)
    private LocalDateTime dataCriacao;



    private LocalDateTime dataFinalizacao;

    @Enumerated(EnumType.STRING)
    private StatusSimulado status;

    private Double pontuacaoFinal;

    // Filtros usados
    @ElementCollection(fetch = FetchType.LAZY)
    @CollectionTable(
            name = "simulado_tema_ids",
            joinColumns = @JoinColumn(name = "simulado_id")
    )
    @Column(name = "tema_ids")
    private List<Long> temaIds;


    @ElementCollection(fetch = FetchType.LAZY)
    @CollectionTable(
            name = "simulado_capitulo_ids",
            joinColumns = @JoinColumn(name = "simulado_id")
    )
    @Column(name = "capitulo_ids")
    private List<Long> capituloIds;


    @ElementCollection(fetch = FetchType.LAZY)
    @CollectionTable(
            name = "simulado_subcapitulo_ids",
            joinColumns = @JoinColumn(name = "simulado_id")
    )
    @Column(name = "subcapitulo_ids")
    private List<Long> subcapituloIds;


    @ElementCollection(fetch = FetchType.LAZY)
    @CollectionTable(
            name = "simulado_bancas",
            joinColumns = @JoinColumn(name = "simulado_id")
    )
    @Column(name = "bancas", length = 255)
    private List<String> bancas;


    @ElementCollection(fetch = FetchType.LAZY)
    @CollectionTable(
            name = "simulado_niveis",
            joinColumns = @JoinColumn(name = "simulado_id")
    )
    @Column(name = "niveis", length = 255)
    private List<String> niveis;


    // Nova: perfil inteligente
    private Boolean inteligente;
    private Boolean balanceado;
    private Boolean prioridadeFraquezas;

}