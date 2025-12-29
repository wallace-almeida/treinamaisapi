package com.treinamaisapi.entity.cartao;

import com.treinamaisapi.entity.baralho.Baralho;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.tema.Tema;
import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.*;
import lombok.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "CARTOES")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Cartao {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(columnDefinition = "TEXT", nullable = false)
    private String frente;

    @Column(columnDefinition = "TEXT", nullable = false)
    private String verso;

    @Column(name = "precisa_revisar")
    private boolean precisaRevisar = true;

    private LocalDateTime ultimaRevisao;
    private Integer repeticoes = 0;
    private Double fatorFacilidade = 2.5;
    private Integer intervaloDias = 1;

    private LocalDateTime proximaRevisao;


    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "tema_id")
    private Tema tema;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "baralho_id")
    private Baralho baralho;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "usuario_id")
    private Usuario usuario;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "questao_id")
    private Questao questao;



}
