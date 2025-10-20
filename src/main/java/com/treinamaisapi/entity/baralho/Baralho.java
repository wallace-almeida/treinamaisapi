package com.treinamaisapi.entity.baralho;

import com.treinamaisapi.entity.cartao.Cartao;
import com.treinamaisapi.entity.tema.Tema;
import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "BARALHOS")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Baralho {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String titulo;

    @CreationTimestamp
    private LocalDateTime dataCriacao;

    @ManyToOne
    @JoinColumn(name = "usuario_id")
    private Usuario usuario;

    @ManyToOne
    @JoinColumn(name = "tema_id")
    private Tema tema;

    @OneToMany(mappedBy = "baralho", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<Cartao> cartoes;
}
