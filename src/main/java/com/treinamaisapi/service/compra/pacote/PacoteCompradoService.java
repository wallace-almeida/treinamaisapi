package com.treinamaisapi.service.compra.pacote;

import com.treinamaisapi.common.dto.compra.response.CompraResponse;
import com.treinamaisapi.common.dto.compra.response.PacoteCompradoComUsuarioDTO;
import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.PacoteCompradoRepository;
import com.treinamaisapi.repository.PacoteRepository;
import com.treinamaisapi.repository.UsuarioRepository;
import org.springframework.transaction.annotation.Transactional;  // ✅ Spring
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Service
@RequiredArgsConstructor
public class PacoteCompradoService {
    private final PacoteCompradoRepository pacoteCompradoRepository;
    private final UsuarioRepository usuarioRepository;
    private final PacoteRepository pacoteRepository;

    @Transactional
    public CompraResponse comprarPacote(Long usuarioId, Long pacoteId) {
        Usuario usuario = usuarioRepository.findById(usuarioId).orElseThrow(() -> new IllegalArgumentException("Usuário não encontrado."));
        Pacote pacote = pacoteRepository.findById(pacoteId).orElseThrow(() -> new IllegalArgumentException("Pacote não encontrado."));

        // Verifica se o usuário já tem um pacote ativo e não expirado

        boolean jaComprado = pacoteCompradoRepository
                .findByUsuarioIdAndAtivoTrue(usuario.getId())
                .stream()
                .anyMatch(c -> c.getPacote().getId().equals(pacote.getId()));

        if (jaComprado) {
            throw new IllegalStateException("Usuário já possui esse pacote ativo");
        }

        LocalDateTime dataExpiracao = LocalDateTime.now().plusDays(pacote.getDuracaoDias());
        // Define a expiração do pacote (exemplo: 30 dias após a compra)
        LocalDate hoje = LocalDate.now();


        PacoteComprado compra = PacoteComprado.builder()
                .usuario(usuario)
                .pacote(pacote)
                .dataCompra(LocalDateTime.now())
                .dataExpiracao(dataExpiracao)
                .ativo(true)
                .build();

        pacoteCompradoRepository.save(compra);

        return CompraResponse.builder()
                .id(compra.getId())
                .usuario(usuario.getNome())
                .pacote(pacote.getNome())
                .ativo(true)
                .dataCompra(compra.getDataCompra())
                .dataExpiracao(compra.getDataExpiracao())
                .build();
    }

    @Transactional(readOnly = true)
    public List<PacoteCompradoComUsuarioDTO> listarComprasAtivas(Long usuarioId) {
        // Busca todos os pacotes ativos do usuário
        List<PacoteComprado> pacotesAtivos = pacoteCompradoRepository.findByUsuarioIdAndAtivoTrue(usuarioId);

        // Converte cada PacoteComprado para PacoteCompradoComUsuarioDTO
        return pacotesAtivos.stream()
                .map(pc -> new PacoteCompradoComUsuarioDTO(
                        pc.getPacote().getId(),
                        pc.getPacote().getNome(),
                        pc.getDataCompra(),
                        pc.getDataExpiracao(),
                        pc.isAtivo(),
                        pc.getUsuario().getId(),
                        pc.getUsuario().getNome(),
                        pc.getUsuario().getEmail(),
                        pc.getPacote().getConcurso().getId()
                ))
                .toList();
    }



    @Transactional
    public void desativarPacoteExpirado(Long pacoteCompradoId) {
        PacoteComprado pacoteComprado = pacoteCompradoRepository.findById(pacoteCompradoId).orElseThrow(() -> new IllegalArgumentException("Pacote não encontrado."));

        if(pacoteComprado.isExpirado()) {
            pacoteComprado.setAtivo(false);
            pacoteCompradoRepository.save(pacoteComprado);
        }

    }

}
