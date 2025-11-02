package com.treinamaisapi.service.tema;

import com.treinamaisapi.common.dto.compra.response.PacoteCompradoComUsuarioDTO;
import com.treinamaisapi.common.dto.questao.request.*;
import com.treinamaisapi.common.dto.questao.response.TemaResponse;
import com.treinamaisapi.entity.capitulo.Capitulo;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.subCapitulo.Subcapitulo;
import com.treinamaisapi.entity.tema.Tema;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.CapituloRepository;
import com.treinamaisapi.repository.SubCapituloRepository;
import com.treinamaisapi.repository.TemaRepository;
import com.treinamaisapi.repository.UsuarioRepository;
import com.treinamaisapi.service.compra.pacote.PacoteCompradoService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class TemaService {

    private final TemaRepository temaRepository;
    private final SubCapituloRepository subCapituloRepository;
    private final CapituloRepository capituloRepository;
    private final PacoteCompradoService pacoteCompradoService;
    private  final UsuarioRepository usuarioRepository;

    public TemaResponse criar (TemaRequest request) {
        // validação simples: não criar duplicado
        if (temaRepository.existsByNomeIgnoreCase(request.getNome())) {
            throw new IllegalArgumentException("Tema já cadastrado: " + request.getNome());
        }
        var tema = Tema.builder().nome(request.getNome()).build();
        temaRepository.save(tema);
        return new TemaResponse( tema.getId(), tema.getNome());
    }

    public Map<String, List<String>> criarTemasEmLote(TemaLoteRequest request) {
        List<String> nomes = request.getTemas().stream()
                .map(TemaRequest::getNome)
                .toList();

        List<String> existentes = temaRepository.findByNomeInIgnoreCase(nomes)
                .stream()
                .map(Tema::getNome)
                .toList();

        List<Tema> novos = nomes.stream()
                .filter(nome -> !existentes.contains(nome))
                .map(nome -> Tema.builder().nome(nome).build())
                .toList();

        temaRepository.saveAll(novos);

        Map<String, List<String>> resultado = new HashMap<>();
        resultado.put("salvos", novos.stream().map(Tema::getNome).toList());
        resultado.put("ignorados", existentes);
        return resultado;
    }

    @Transactional
    public Map<String, List<String>> criarEstruturaCompleta(EstruturaLoteRequest request) {
        List<String> salvos = new ArrayList<>();
        List<String> ignorados = new ArrayList<>();

        for (EstruturaHierarquicaRequest temaReq : request.getTemas()) {
            Tema tema = temaRepository.findByNomeIgnoreCase(temaReq.getNome())
                    .orElseGet(() -> {
                        Tema novo = temaRepository.save(Tema.builder().nome(temaReq.getNome()).build());
                        salvos.add("Tema: " + novo.getNome());
                        return novo;
                    });

            for (CapituloLoteRequest capReq : temaReq.getCapitulos()) {

                Capitulo capitulo = capituloRepository.findByNomeIgnoreCaseAndTema_Id(capReq.getNome(), tema.getId())
                        .orElseGet(() -> {
                            Capitulo novoCap = capituloRepository.save(
                                    Capitulo.builder()
                                            .nome(capReq.getNome())
                                            .tema(tema)
                                            .build()
                            );
                            salvos.add("Capítulo: " + novoCap.getNome());
                            return novoCap;
                        });

                for (String subNome : capReq.getSubcapitulos()) {
                    if (subCapituloRepository.existsByNomeIgnoreCaseAndCapitulo_Id(subNome, capitulo.getId())) {
                        ignorados.add("Subcapítulo existente: " + subNome);
                    } else {
                        subCapituloRepository.save(
                                Subcapitulo.builder()
                                        .nome(subNome)
                                        .capitulo(capitulo)
                                        .build()
                        );
                        salvos.add("Subcapítulo: " + subNome);
                    }
                }
            }
        }

        Map<String, List<String>> resultado = new HashMap<>();
        resultado.put("salvos", salvos);
        resultado.put("ignorados", ignorados);
        return resultado;
    }

    public List<TemaResponse> listar () {
        return temaRepository.findAll().stream().map(
                t -> new TemaResponse(t.getId(), t.getNome())
        ).toList();
    }

    public List<Tema> listarTemasDisponiveis(Long usuarioId) {
        Usuario usuario = usuarioRepository.findById(usuarioId)
                .orElseThrow(() -> new RuntimeException("Usuário não encontrado."));

        List<PacoteCompradoComUsuarioDTO> comprasAtivas = pacoteCompradoService.listarComprasAtivas(usuarioId);

        return comprasAtivas.stream()
                .flatMap(c -> temaRepository.findByPacotes_Id(c.getPacoteId()).stream())
                .distinct()
                .toList();
    }


}
