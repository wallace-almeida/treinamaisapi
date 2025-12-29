package com.treinamaisapi.entity.avatar;



import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "avatar")
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class Avatar {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * Código lógico do avatar (ex: DEFAULT_1, STUDENT_1)
     * Usado pelo sistema e frontend
     */
    @Column(nullable = false, unique = true)
    private String nome;

    /**
     * Caminho da imagem (asset ou URL)
     */
    @Column(nullable = false)
    private String caminhoImagem;

    /**
     * Controle administrativo
     */
    @Column(nullable = false)
    private Boolean ativo = true;
}
